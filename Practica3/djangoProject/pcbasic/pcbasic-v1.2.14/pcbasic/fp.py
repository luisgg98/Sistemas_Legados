"""
PC-BASIC - fp.py
MBF Floating-point arithmetic

(c) 2013, 2014, 2015 Rob Hagemans
This file is released under the GNU GPL version 3.
"""

# descriptions of the Microsoft Binary Format found here:
# http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_20245266.html
# http://www.boyet.com/Articles/MBFSinglePrecision.html
#
# single precision:                      m3 | m2 | m1 | exponent
# double precision:  m7 | m6 | m5 | m4 | m3 | m2 | m1 | exponent
# where:
#     m1 is most significant byte => sbbb|bbbb
#     m7 is the least significant byte
#     m = mantissa byte
#     s = sign bit
#     b = bit
#
# The exponent is biased by 128.
# There is an assumed 1 bit after the radix point (so the assumed mantissa is 0.1ffff... where f's are the fraction bits)

import math
from functools import partial

import error
import state

# the exponent is biased by 128
true_bias = 128

######################################

# calculation exception states
state.basic_state.overflow = False
state.basic_state.zero_div = False
# stream for Division by Zero and Overflow messages
errstream = None

def init(error_stream):
    """ Initialise the Floating-point system. """
    global errstream
    errstream = error_stream

def msg_overflow():
    """ Write an Overflow message. """
    if state.basic_state.overflow:
        return
    state.basic_state.overflow = True
    math_error(6)

def msg_zero_div():
    """ Write a Division by Zero message. """
    if state.basic_state.zero_div:
        return
    state.basic_state.zero_div = True
    math_error(11)

def math_error(errnum):
    """ Write an error message; only break execution if a handler is active. """
    if state.basic_state.on_error:
        # also raises exception in error_handle_mode! in that case, prints a normal error message
        raise(error.RunError(errnum))
    else:
        # write a message & continue as normal
        # start_line() ?
        errstream.write_line(error.get_message(errnum)) # no space, no line number

####################################

class Float(object):
    """ Floating-point number in Microsoft Binary Format. """

    # class variables, to override
    digits = None
    mantissa_bits = None
    byte_size = None
    bias = None
    carry_mask = None
    # constants
    zero = None
    one = None
    ten = None
    half = None
    max = None

    def __init__(self, neg=False, man=0, exp=0):
        """ Initialise float. """
        self.neg, self.man, self.exp = neg, man, exp

    def copy(self):
        """ Clone float. """
        return self.__class__(self.neg, self.man, self.exp)

    @classmethod
    def from_int(cls, num):
        """ Convert int to float. """
        # this creates an mbf float. the carry byte will also be in use. call discard_carry afterwards if you want an empty carry.
        # set mantissa to number, shift to create carry bytes
        n = cls( (num<0), long(abs(num) << 8), cls.bias )
        # normalise shifts to turn into proper mbf
        n.normalise()
        return n

    @classmethod
    def from_bytes(cls,s):
        """ Convert byte representation to float. """
        # put mantissa in form . 1 f1 f2 f3 ... f23
        # internal representation has four bytes, last byte is carry for intermediate results
        # put mantissa in form . 1 f1 f2 f3 ... f55
        # internal representation has seven bytes, last bytes are carry for intermediate results
        man = long((s[-2]|0x80) * 0x100**(cls.byte_size-2))
        for i in range(cls.byte_size-2):
            man += s[-cls.byte_size+i] * 0x100**i
        man <<= 8
        return cls( (s[-2] >= 0x80), man, s[-1])

    def to_bytes(self):
        """ Convert float to byte representation. """
        self.apply_carry()

        if self.exp > 0xff:
            # overflow
            # message does not break execution, no line number
            msg_overflow()
            self.exp = 0xff
            self.man = self.carry_mask #0xffffffffffffff00L

        # extract bytes
        s = bytearray()
        man = self.man
        for _ in range(self.byte_size-1):
            man >>= 8
            s.append(man&0xff)
        # append exponent byte
        s.append(self.exp)
        # apply sign
        s[-2] &= 0x7f
        if (self.neg):
            s[-2] |= 0x80
        return s

    def is_zero(self):
        """ Check if float equals zero. """
        return self.exp==0

    def sign(self):
        """ Return sign of float. """
        if self.exp==0:
            return 0
        elif self.neg:
            return -1
        else:
            return 1

    def apply_carry(self):
        """ Apply the carry byte. """
        # carry bit set? then round up
        if (self.man & 0xff) > 0x7f:
            self.man += 0x100
        # overflow?
        if self.man >= 0x100**self.byte_size:
            self.exp +=1
            self.man >>= 1
        # discard carry
        self.man ^= (self.man&0xff)
        return self

    def discard_carry(self):
        """ Discard the carry byte. """
        self.man ^= (self.man&0xff)
        return self

    def trunc_to_int(self):
        """ Truncate float to integer. """
        man = self.man >> 8
        if self.exp > self.bias :
            val = long(man << (self.exp-self.bias))
        else:
            val = long(man >> (-self.exp+self.bias))
        if self.neg:
            return -val
        else:
            return val

    def round_to_int(self):
        """ Round float to integer. """
        if self.exp > self.bias:
            man = long(self.man << (self.exp-self.bias))
        else:
            man = long(self.man >> (-self.exp+self.bias))
        # carry bit set? then round up (affect mantissa only, note we can be bigger than our byte_size allows)
        #if (n_in.man & 0xff) > 0x7f:
        if (man & 0xff) > 0x7f:
            man += 0x100
        if self.neg:
            return -(man >> 8)
        else:
            return (man >> 8)

    def normalise(self):
        """ Bring float to normal form. """
        # zero mantissa -> make zero
        if self.man == 0 or self.exp == 0:
            self.neg, self.man, self.exp = self.zero.neg, self.zero.man, self.zero.exp
            return self
        # are these correct?
        while self.man <= 2**(self.mantissa_bits+8-1): # 0x7fffffffffffffff: # < 2**63
            self.exp -= 1
            self.man <<= 1
        while self.man > 2**(self.mantissa_bits+8): #0xffffffffffffffff: # 2**64 or 0x100**8
            self.exp += 1
            self.man >>= 1
        # underflow
        if self.exp < 0:
            self.exp = 0
        # overflow
        if self.exp > 0xff:
            # overflow
            # message does not break execution, no line number
            msg_overflow()
            self.exp = 0xff
            self.man = self.carry_mask #0xffffffffffffff00L
        return self

    def itrunc(self):
        """ In-place. Discard carry & truncate towards zero; return as float. """
        self = self.from_int(self.trunc_to_int())
        return self

    def ifloor(self):
        """ In-place. Discard carry & truncate towards -infinity; return as float. """
        if self.is_zero():
            return self
        n = self.from_int(self.trunc_to_int())
        if n.neg and not self.equals(n):
            self = sub(n, n.one)
        else:
            self = n
        return self

    def iround(self):
        """ In-place. Round and return as float. """
        if self.exp-self.bias > 0:
            self.man = long(self.man * 2**(self.exp-self.bias))
        else:
            self.man = long(self.man / 2**(-self.exp+self.bias))
        self.exp = self.bias
        # carry bit set? then round up (moves exponent on overflow)
        self.apply_carry()
        self.normalise()
        return self

    def negate(self):
        """ In-place negation. """
        self.neg = not self.neg
        return self

    def iadd_raw(self, right_in):
        """ Unnormalised add in-place. """
        if right_in.is_zero():
            return self
        if self.is_zero():
            self.neg, self.man, self.exp = right_in.neg, right_in.man, right_in.exp
            return self
        # ensure right has largest exponent
        if self.exp > right_in.exp:
            right = self.copy()
            self.neg, self.man, self.exp = right_in.neg, right_in.man, right_in.exp
        else:
            right = right_in
        # denormalise left to match exponents
        while self.exp < right.exp:
            self.exp += 1
            self.man >>= 1
        # add mantissas, taking sign into account
        if (self.neg == right.neg):
            self.man += right.man
        else:
            if self.man > right.man:
                self.man -= right.man
            else:
                self.man = right.man - self.man
                self.neg = right.neg
        return self

    def iadd(self, right):
        """ In-place addition. """
        return self.iadd_raw(right).normalise()

    def isub(self, right_in):
        """ In-place subtraction. """
        return self.iadd(self.__class__(not right_in.neg, right_in.man, right_in.exp))

    def imul10(self):
        """ In-place multiplication by 10. """
        if self.is_zero():
            return self
        # 10x == 2(x+4x)
        n = self.__class__(self.neg, self.man, self.exp+2)
        self.iadd_raw(n)
        self.exp += 1
        self.normalise()
        return self

    def imul(self, right_in):
        """ In-place multiplication. """
        if self.is_zero():
            return self
        if right_in.is_zero():
            self.neg, self.man, self.exp = right_in.neg, right_in.man, right_in.exp
            return self
        self.exp += right_in.exp - right_in.bias - 8
        self.neg = (self.neg != right_in.neg)
        self.man = long(self.man * right_in.man)
        self.normalise()
        return self

    def isq(self):
        """ In-place square. """
        self.imul(self)
        return self

    def idiv(self, right_in):
        """ In-place division. """
        if right_in.is_zero():
            msg_zero_div()
            self.man, self.exp = self.max.man, self.max.exp
            return self
        if self.is_zero():
            return self
        # signs
        self.neg = (self.neg != right_in.neg)
        # subtract exponentials
        self.exp -= right_in.exp - right_in.bias - 8
        # long division of mantissas
        work_man = self.man
        denom_man = right_in.man
        self.man = 0L
        self.exp += 1
        while (denom_man > 0):
            self.man <<= 1
            self.exp -= 1
            if work_man > denom_man:
                work_man -= denom_man
                self.man += 1L
            denom_man >>= 1
        self.normalise()
        return self

    def idiv10(self):
        """ In-place division by 10. """
        self.idiv(self.ten)
        return self

    def ipow_int(self, expt):
        """ In-place exponentiation by integer. """
        # exponentiation by squares
        if expt < 0:
            self.ipow_int(-expt)
            self = div(self.one, self)
        elif expt > 1:
            if (expt%2) == 0:
                self.ipow_int(expt/2)
                self.isq()
            else:
                base = self.copy()
                self.ipow_int((expt-1)/2)
                self.isq()
                self.imul(base)
        elif expt == 0:
            self = self.one.copy()
        return self

    def abs_gt(self, right):
        """ Absolute value is greater than. """
        if self.exp != right.exp:
            return (self.exp > right.exp)
        return (self.man > right.man)

    def gt(self, right):
        """ Greater than. """
        if self.neg != right.neg:
            return right.neg
        elif self.neg:
            return right.abs_gt(self)
        else:
            return self.abs_gt(right)

    def equals(self, right):
        """ Float equals other float. """
        if self.is_zero():
            # all zeroes are equal
            return right.is_zero()
        return (self.neg==right.neg and self.exp==right.exp and self.man&self.carry_mask == right.man&right.carry_mask)

    def bring_to_range(self, lim_bot, lim_top):
        """ Return exponentiation needed to bring float into range. """
        exp10 = 0
        while self.abs_gt(lim_top):
            self.idiv10()
            exp10 += 1
        self.apply_carry()
        while lim_bot.abs_gt(self):
            self.imul10()
            exp10 -= 1
        # round off carry byte before doing the decimal rounding
        # this brings results closer in line with GW-BASIC output
        self.apply_carry()
        ##self.discard_carry()
        # round to integer: first add one half
        self.iadd(self.half)
        ##self.apply_carry()
        # then truncate to int (this throws away carry)
        num = abs(self.trunc_to_int())
        # round towards neg infinity when negative
        if self.neg:
            num += 1
        return num, exp10

    def to_value(self):
        """ Convert to Python float. """
        if self.is_zero():
            return 0.0
        self.apply_carry()
        man = self.man >> 8
        return man * 2**(self.exp - self.bias) * (1-2*self.neg)

    @classmethod
    def from_value(cls, value):
        """ Set to value of Python float. """
        if value == 0.0:
            return cls.zero
        neg = value < 0
        fexp = math.log(abs(value), 2) - cls.mantissa_bits
        exp = int(fexp)
        man = int(abs(value) * 0.5**(exp-8))
        exp += cls.bias
        return cls(neg, man, exp).normalise()


class Single(Float):
    """ Single-precision float. """
    digits = 7
    mantissa_bits = 24
    byte_size = 4
    bias = true_bias + mantissa_bits
    carry_mask = 0xffffff00


class Double(Float):
    """ Double-precision float. """
    digits = 16
    mantissa_bits = 56
    byte_size = 8
    bias = true_bias + mantissa_bits
    carry_mask = 0xffffffffffffff00

    def round_to_single(self):
        """ Round double to single. """
        mybytes = self.to_bytes()
        single = Single.from_bytes(mybytes[4:])
        single.man += mybytes[3]
        return single.normalise()


####################################

def from_bytes(s):
    """ Convert byte sequence to single or double. """
    if len(s) == 4:
        return Single.from_bytes(s)
    elif len(s) == 8:
        return Double.from_bytes(s)

def unpack(value):
    """ Unpack a float for manipulation. """
    state.basic_state.overflow = False
    state.basic_state.zero_div = False
    return from_bytes(value[1])

def pack(n):
    """ Pack a float into BASIC representation. """
    s = n.to_bytes()
    if len(s) == 8:
        return ('#', s)
    elif len(s) == 4:
        return ('!', s)


####################################
# standalone arithmetic operators

def add(left_in, right_in):
    """ Add two floats. """
    return left_in.copy().iadd(right_in)

def sub(left_in, right_in):
    """ Subtract two floats. """
    return left_in.copy().isub(right_in)

def mul(left_in, right_in):
    """ Multiply two floats. """
    return left_in.copy().imul(right_in)

def div(left_in, right_in):
    """ Divide two floats. """
    return left_in.copy().idiv(right_in)

def sq(n):
    """ Square a float. """
    return mul(n, n)

def pow_int(left_in, right_in):
    """ Raise a float to an integer power. """
    return left_in.copy().ipow_int(right_in)

####################################
# math function

def safe(fn, *args):
    """ Convert to IEEE 754, apply function, convert back. """
    try:
        return args[0].__class__().from_value(fn(*(arg.to_value() for arg in args)))
    except OverflowError:
        msg_overflow()
        return args[0].__class__(args[0].neg, args[0].carry_mask, 0xff)
    except ZeroDivisionError:
        msg_zero_div()
        return args[0].__class__(args[0].neg, args[0].carry_mask, 0xff)
    except ValueError:
        raise error.RunError(error.IFC)

power = partial(safe, lambda a,b: a**b)
sqrt = partial(safe, math.sqrt)
exp  = partial(safe, math.exp)
sin  = partial(safe, math.sin)
cos  = partial(safe, math.cos)
tan  = partial(safe, math.tan)
atn  = partial(safe, math.atan)
log  = partial(safe, math.log)


####################################
# constants

Single.zero     = from_bytes(bytearray('\x00\x00\x00\x00'))
Single.half     = from_bytes(bytearray('\x00\x00\x00\x80'))
Single.one      = from_bytes(bytearray('\x00\x00\x00\x81'))
Single.two      = from_bytes(bytearray('\x00\x00\x00\x82'))
Single.ten      = from_bytes(bytearray('\x00\x00\x20\x84'))
Single.max      = from_bytes(bytearray('\xff\xff\x7f\xff'))
Single.e        = from_bytes(bytearray('\x54\xf8\x2d\x82'))
Single.pi       = from_bytes(bytearray('\xdb\x0f\x49\x82'))
Single.log2     = from_bytes(bytearray('\x16\x72\x31\x80'))    # rounding not correct but extracted from GW-BASIC
Single.twopi    = mul(Single.pi, Single.two)
Single.pi2      = mul(Single.pi, Single.half)
Single.pi4      = mul(Single.pi2, Single.half)

Double.zero     = from_bytes(bytearray('\x00\x00\x00\x00\x00\x00\x00\x00'))
Double.half     = from_bytes(bytearray('\x00\x00\x00\x00\x00\x00\x00\x80'))
Double.one      = from_bytes(bytearray('\x00\x00\x00\x00\x00\x00\x00\x81'))
Double.two      = from_bytes(bytearray('\x00\x00\x00\x00\x00\x00\x00\x82'))
Double.ten      = from_bytes(bytearray('\x00\x00\x00\x00\x00\x00\x20\x84'))
Double.max      = from_bytes(bytearray('\xff\xff\xff\xff\xff\xff\x7f\xff'))
Double.e        = from_bytes(bytearray('\x4b\xbb\xa2\x58\x54\xf8\x2d\x82'))
Double.pi       = from_bytes(bytearray('\xc2\x68\x21\xa2\xda\x0f\x49\x82'))
Double.log2     = from_bytes(bytearray('\x7a\xcf\xd1\xf7\x17\x72\x31\x80'))
Double.twopi    = mul(Double.pi, Double.two)
Double.pi2      = mul(Double.pi, Double.half)
Double.pi4      = mul(Double.pi2, Double.half)
