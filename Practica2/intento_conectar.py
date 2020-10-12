from py3270 import Emulator

#Web de la biblioteca
# https://www.suhendro.com/2019/11/ibm-3270-screen-scraping-and-automation/

# use x3270 so you can see what is going on
em = Emulator(visible=True)

# or not (uses s3270)
#Esto es lo que vamos a usar en las practicas
#em = Emulator()


#connect(host):
    #Description: Connect to a host
    #host (string): host name or IP address
em.connect('155.210.152.51:3270')

#exec_command(cmdstr):
    #Description: Execute an x3270 command 'cmdstr' gets sent directly to the x3270 subprocess on it is stdin.
    # Alternatively, there is some frequently used keys shortcut:

#send_enter() is equivalent to exec_command(b"Enter")
#em.send_enter()

#send_pf3() is equivalent to exec_command(b"PF(3)")
    #etc.
    #Arguments:
    #cmdstr (string): x3270 command

#wait_for_field():
    #Description: Wait until the screen is ready, the cursor has been positioned on a modifiable field, and the keyboard is unlocked. Sometimes the server will "unlock" the keyboard but the screen will not yet be ready. In that case, an attempt to read or write to the screen will result in a 'E' keyboard status because we tried to read from a screen that is not yet ready. Using this method tells the client to wait until a field is detected and the cursor has been positioned on it.
    #Arguments: none
    # if your host unlocks the keyboard before truly being ready you can use:

#Si esto lo pones al hacer click termina
#em.wait_for_field()



#string_get(ypos, xpos, length):
    #Description: Get a string of 'length' at screen coordinates 'ypos'/'xpos'. Coordinates are 1 based, as listed in the status area of the terminal.
    #Arguments:
    #ypos (int): y position (row number)
    #xpos (int): x position (column number)
    #length (int): length of the string
dato= em.string_get(1,1,14)
print dato

#string_found(ypos, xpos, string):
#Description: Return True if 'string' is found at screen coordinates 'ypos'/'xpos', False otherwise. Coordinates are 1 based, as listed in the status area of the terminal.
#Arguments:
#ypos (int): y position (row number)
#xpos (int): x position (column number)
#length (int): length of the string
encuentro_algo= em.string_found(14,14,"HHHHHHH")
print encuentro_algo

#move_to(ypos, xpos):
#Description: move the cursor to the given coordinates. Coordinates are 1 based, as listed in the status area of the terminal.
#Arguments:
#ypos (int): y position (row number)
#xpos (int): x position (column number)
em.move_to(7,7)

#send_string(tosend, ypos=none, xpos=none):
#Description: Send a string to the screen at the current cursor location or at screen coordinates 'ypos'/'xpos' if they are both given. Coordinates are 1 based, as listed in the status area of the terminal.
#Arguments:
#tosend (string): string to be sent to the screen
#ypos (int): y position (row number)
#xpos (int): x position (column number)

#fill_field(ypos, xpos, tosend, length):
#Description: Clears the field at the position given and inserts the string 'tosend'. Coordinates are 1 based, as listed in the status area of the terminal. Raises: FieldTruncateError if 'tosend' is longer than 'length'.
#Arguments:
#ypos (int): y position (row number)
#xpos (int): x position (column number)
#tosend: the string to insert
#length: the length of the field

#delete_field():
#Description: Delete contents in field at current cursor location and positions, cursor at beginning of field.
#Arguments: none

#save_screen(file_path):
#Description: Save the current screen as a file.
#Arguments:
#file_path (string): file path and name
#Guarda la pantalla en el formato que le pongas
em.save_screen("/home/luisgg/Documents/Sistemas_Legados/practicas/Sistemas_Legados/file.txt")
em.save_screen("/home/luisgg/Documents/Sistemas_Legados/practicas/Sistemas_Legados/file.html")
em.save_screen("/home/luisgg/Documents/Sistemas_Legados/practicas/Sistemas_Legados/file")


#terminate():
    #Description: terminates the underlying x3270 subprocess. Once called, this Emulator instance must no longer be used.
    #Arguments: none
    # disconnect from host and kill subprocess
em.terminate()