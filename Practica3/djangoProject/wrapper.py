import pyautogui
import re
import time
import os
import win32gui
import subprocess


class WindowMgr():
    ###########################################
    ##      VARIABLES PRIVADAS               ##
    ###########################################
    __path = "C:\\Legados\\"


    ###########################################
    ##      FUNCIONES PRIVADAS               ##
    ###########################################

    """FUNCIONES PRIVADAS"""

    def __window_enum_callback(self, hwnd, wildcard):
        """Pasar a win32gui.EnumWindows() para comprobar todas las ventanas abiertas"""
        if re.match(wildcard, str(win32gui.GetWindowText(hwnd))) is not None:
            self._handle = hwnd


    def __find_window_wildcard(self, wildcard):
        """Encuentra una ventana segun su nombre mediante una expresion"""
        self._handle = None
        win32gui.EnumWindows(self.__window_enum_callback, wildcard)


    def __screenshot(self):
        """Toma una captura de la ventana guardada actual. No debe estar minimizada"""
        win32gui.SetForegroundWindow(self._handle)
        x, y, x1, y1 = win32gui.GetClientRect(self._handle)
        x, y = win32gui.ClientToScreen(self._handle, (x, y))
        x1, y1 = win32gui.ClientToScreen(self._handle, (x1 - x, y1 - y))
        pyautogui.screenshot('menu.png', region=(x, y, x1, y1))


    def __prepare_aplication(self):
        """Lanza la aplicacion legada"""
        # Abrimos la aplicación y damos tiempo a que se inicie
        subprocess.Popen([self.__path + "DOSBox-0.74\DOSBox.exe", self.__path + "Database\gwbasic.bat", "-noconsole"])
        time.sleep(6)
        # Make screenshot of Menu
        self.__find_window_wildcard('.*DOSBox.*')
        self.__screenshot()


    def __prepare_pyautogui(self):
        """Prepara la ventana correctamente para enviar cosas con pyautogui"""
        window = pyautogui.getWindowsWithTitle("DOSBox")[0]
        window.minimize()
        window.restore()


    def __in_menu(self):
        """Devuelve true si estamos en la pantalla de menu"""
        if pyautogui.locateOnScreen("menu.png"): return True
        else: return False


    def __get_info_in_file(self):
        """Hace que la aplicacion legada muestre todos los registros"""
        resultado = []
        while not self.__in_menu():
            i = 0
            while i < 20:
                pyautogui.press("space")
                i = i + 1
                time.sleep(0.20)

        # Hemos leido todos los datos y están en el fichero así que cerramos la aplicación legada
        pyautogui.hotkey("ctrl","f9")
        time.sleep(5)


    def __read_info_from_file(self):
        """Lectura del fichero donde esta la salida de la aplicacion legada (que ya esta apagada)"""
        resultado = []
        f = open(self.__path + "Database\FILE.txt", "r")
        lines = f.read().splitlines()
        f.close()
        # Eliminamos lineas innecesarias
        for x in lines:
            if x[:5] != "PULSA":
                resultado = resultado + [x]
            else:
                resultado = resultado + ["n"]
        resultado = resultado[15:len(resultado) - 9]
        resultado = ["n"] + resultado
        os.remove(self.__path + "Database\FILE.txt" )
        return resultado


    def __parse_all_columns(self, resultado,cinta):
        """Generamos una lista de listas con los campos [[nombre,tipo,cinta,registro]]"""
        data = []
        if len(resultado) % 5 == 0:
            i = 0
            while i < len(resultado):
                if resultado[i+3] == cinta:
                    r4 = resultado[i + 4].replace(" ", "")
                    e = [resultado[i + 1], resultado[i + 2], resultado[i + 3], r4]
                    data = data + [e]
                i = i + 5
        return data


    def __parse_name(self,resultado):
        """Devolvemos una lista con los nombres de los registros del sistema"""
        data = []
        if len(resultado) % 5 == 0:
            i = 0
            while i < len(resultado):
                data = data + [resultado[i + 1]]
                i = i + 5
        return data


    def __find_register(self,programa):
        """Buscamos en la aplicacion legada el registro con el nombre programa"""
        pyautogui.typewrite("7")
        pyautogui.typewrite("N")
        pyautogui.press("enter")
        pyautogui.write(programa, interval=0.15)
        pyautogui.press("enter")
        time.sleep(2)
        pyautogui.press("enter")
        pyautogui.press("enter")
        pyautogui.press("enter")
        # Hemos leido el dato y esta en el fichero así que cerramos la aplicación legada
        pyautogui.hotkey("ctrl", "f9")
        time.sleep(5)


    def __read_found_register_file(self):
        """Leemos del fichero los datos del registro con nombre que hemos buscado"""
        # Lectura del fichero
        f = open(self.__path + "Database\FILE.txt", "r")
        lines = f.read().splitlines()
        f.close()
        i = 0
        found = False

        # Buscamos asi la linea porque no siempre aparece el resultado en la misma linea
        while i < len(lines) and not found:
            aux = lines[i]
            if aux[:7] == "DIME EL":
                found = True
            else:
                i = i + 1
        # Borramos el fichero
        os.remove(self.__path + "Database\FILE.txt")
        return lines[i+1]


    def __parse_register(self,register):
        """Si existia un registro con ese nombre devuelve [nombre,tipo,cinta,registro]
            Si no escistia devuelve una lista vacia []"""
        data = []
        if register[:13] != "NO HAY NINGUN":
            # Sustituyo cuando hay dos o mas espacios seguidos por tabulador y arreglo los resultados
            aux = re.sub("\\s\s+", "\t", register)
            aux = aux.split("\t")
            aux[0] = aux[0].replace(" ","")
            aux[1] = aux[1].replace("- ","")
            aux[3] = aux[3].replace("CINTA:","")

            # Transformacion a [[nombre],[tipo],[cinta],[registro]]
            data = [aux[1]] + [aux[2]] + [aux[3]] + [aux[0]]
        return data


    ###########################################
    ##      FUNCIONES PUBLICAS              ##
    ###########################################

    """ BUSCAR DADO EL NOMBRE LA INFORMACION DEL REGISTRO 
        Devuelve:   [nombre, tipo, cinta, registro] si existe
                    [] si no existe """

    def find_program_by_name(self, programa):
        # Lanzamos la aplicacion legada
        self.__prepare_aplication()

        # Preparamos pyautogui para poder enviar letras/teclas
        #self.__prepare_pyautogui()

        programa = programa.upper()
        # Busca en la apliacion legada
        self.__find_register(programa)

        # Lee el fichero con la salida de la aplicacion legada
        register = self.__read_found_register_file()

        # Prepara la devolucion en el formato adecuado
        result = self.__parse_register(register)

        return result



    """ LISTADO DE TODOS LOS REGISTROS DEL SISTEMA
        DEVUELVE LISTA DE LISTAS:
        [[nombre, tipo, cinta, registro]] """

    def get_all_programs_tape(self,cinta):
        # Lanzamos la aplicacion legada
        self.__prepare_aplication()

        # Preparamos pyautogui para poder enviar letras/teclas
        #self.__prepare_pyautogui()

        # Accedemos al listado
        pyautogui.typewrite("6")
        pyautogui.press("enter")

        # La aplicacion legada lista todos los registros
        self.__get_info_in_file()

        # Leemos el listado del fichero de la salida de la apliacion legada
        resultado = self.__read_info_from_file()

        # Prepara la devolucion en el formato adecuado
        data = self.__parse_all_columns(resultado,cinta)
        return data



    """ LISTADO DE TODOS LOS NOMBRES DE REGISTROS DEL SISTEMA
           DEVUELVE LISTA DE LISTAS:
           [nombre1,nombre2,...] """

    def get_name_programs(self):
        # Lanzamos la aplicacion legada
        self.__prepare_aplication()

        # Preparamos pyautogui para poder enviar letras/teclas
        #self.__prepare_pyautogui()

        # Accedemos al listado
        pyautogui.typewrite("6")
        pyautogui.press("enter")

        # La aplicacion legada lista todos los registros
        self.__get_info_in_file()

        # Leemos el listado del fichero de la salida de la apliacion legada
        resultado = self.__read_info_from_file()

        # Prepara la devolucion en el formato adecuado
        data = self.__parse_name(resultado)
        return data


#w = WindowMgr()
#list = w.get_all_programs_tape("B")
#print("hola")
#print(str(len(list)))
#for i in list:
#    print(i)

