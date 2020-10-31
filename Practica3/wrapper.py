import pyautogui
import pytesseract
from PIL import Image
from PIL import ImageGrab
import win32gui
import win32con
import re


class WindowMgr:
    """Permite encontrar una ventana y almacenarla para hacer screenshot"""

    def __init__(self):
        """Constructor"""
        self._handle = None

    def _window_enum_callback(self, hwnd, wildcard):
        """Pasar a win32gui.EnumWindows() para comprobar todas las ventanas abiertas"""
        if re.match(wildcard, str(win32gui.GetWindowText(hwnd))) is not None:
            self._handle = hwnd

    def find_window_wildcard(self, wildcard):
        """Encuentra una ventana segun su nombre mediante una expresion"""
        self._handle = None
        win32gui.EnumWindows(self._window_enum_callback, wildcard)

    def screenshot(self):
        """Toma una captura de la ventana guardada actual. No debe estar minimizada"""
        win32gui.SetForegroundWindow(self._handle)
        x, y, x1, y1 = win32gui.GetClientRect(self._handle)
        x, y = win32gui.ClientToScreen(self._handle, (x, y))
        x1, y1 = win32gui.ClientToScreen(self._handle, (x1 - x, y1 - y))
        pyautogui.screenshot('images/captura.png', region=(x, y, x1, y1))

# Conseguir la pantalla de la aplicacion legada
w = WindowMgr()
w.find_window_wildcard('.*DOSBox.*')

# Hacer captura de la ventana de la aplicacion legada
w.screenshot()

# Setup de tesseract
pytesseract.pytesseract.tesseract_cmd = r'C:\Program Files\Tesseract-OCR\tesseract'

# Obtener texto de la captura
print(pytesseract.image_to_string(Image.open('images/captura.png'), lang='spa'))
