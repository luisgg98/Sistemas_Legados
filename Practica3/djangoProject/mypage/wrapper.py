import argparse

import cv2
import pyautogui
import pytesseract
from PIL import Image
import win32gui
import re
import time




class WindowMgr():
    """Permite encontrar una ventana y almacenarla para hacer screenshot"""

    ###########################################
    ##      FUNCIONES PRIVADAS               ##
    ###########################################

    """FUNCIONES PRIVADAS"""

    def __init__(self):
        """Constructor"""
        self._handle = None

    def __check_type_program(self, programa):
        """Verifica cual es el tipo del programa"""
        tipo_encontrado = "Indeterminado"
        # Ya que el numero de tipos es finito, compruebo manualmente
        # a cual se corresponde, lo elimino de la descripcion del programa
        # y devuelvo ambos
        x = re.search("\sVIDE[A-Z]*A\s", programa)
        if x:
            programa = re.sub("\sVIDE[A-Z]*A\s", "", programa)
            return ["VIDEOAVENTURA", programa]

        x = re.search("\sSIM[A-Z]*R\s", programa)
        if x:
            programa = re.sub("\sSIM[A-Z]*R\s", "", programa)
            return ["SIMULADOR", programa]

        x = re.search("\sARCADE\s", programa)
        if x:
            programa = re.sub("\sARCADE\s", "", programa)
            return ["ARCADE", programa]

        x = re.search("\sCONV[A-Z]*\s", programa)
        if x:
            programa = re.sub("\sCONV[A-Z]*\s", "", programa)
            return ["CONVERSACIONAL", programa]

        x = re.search("\sJUEGO DE MESA\s", programa)
        if x:
            programa = re.sub("\sJUEGO DE MESA\s", "", programa)
            return ["JUEGO DE MESA", programa]

        x = re.search("\sS[A-Z]* D[A-Z]*TIVO\s", programa)
        if x:
            programa = re.sub("\sS[A-Z]* D[A-Z]*TIVO\s", "", programa)
            return ["S. DEPORTIVO", programa]

        x = re.search("\sESTR[A-Z]*A\s", programa)
        if x:
            programa = re.sub("\sESTR[A-Z]*A\s", "", programa)
            return ["ESTRATEGIA", programa]

        return [tipo_encontrado, programa]

    def start_project(self):
        """Funcion para centrarse en la aplicacion legada """
        self.find_window_wildcard('.*DOSBox.*')
        pyautogui.press('alt')
        win32gui.SetForegroundWindow(self._handle)
        # Es necesario para forzarlo a centrarse en la aplicacion
        pyautogui.press('enter')
        pytesseract.pytesseract.tesseract_cmd = r'C:\Program Files\Tesseract-OCR\tesseract'

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
        # win32gui.SetForegroundWindow(self._handle)
        x, y, x1, y1 = win32gui.GetClientRect(self._handle)
        x, y = win32gui.ClientToScreen(self._handle, (x, y))
        x1, y1 = win32gui.ClientToScreen(self._handle, (x1 - x, y1 - y))
        pyautogui.screenshot('images/captura.png', region=(x, y, x1, y1))
        mucho_texto = (pytesseract.image_to_string(Image.open('images/captura.png'), lang='spa'))
        return (mucho_texto.split('\n'))

    def screenshot_only(self):
        """Toma una captura de la ventana guardada actual. No debe estar minimizada"""
        # win32gui.SetForegroundWindow(self._handle)
        x, y, x1, y1 = win32gui.GetClientRect(self._handle)
        x, y = win32gui.ClientToScreen(self._handle, (x, y))
        x1, y1 = win32gui.ClientToScreen(self._handle, (x1 - x, y1 - y))
        pyautogui.screenshot('images/captura.png', region=(x, y, x1, y1))

    def __clean_list(self, datas):
        """Elimina los elementos innecesarios de la lista"""
        i = 0
        new_list = []
        while i < (len(datas) - 1):
            # Elimino las lineas de la pantalla que son o un espacio o cadena vacia
            if datas[i] != '' and datas[i] != ' ':
                new_list.append(datas[i])
            else:
                pass
            # me quedo solo con el texto que pueda ser util
            i = i + 1
        return new_list

    # Funcion utilizada para buscar un programa por su nombre
    def __filter_result(self, data):
        """ Data una lista de elementos se queda con el que empieza por un digito"""
        i = 0
        new_list_list = []
        while i < len(data):
            # Esto es debido a que los elementos de los programas cuando los buscas por
            # nombre te los muestra 1 por 1 y todos empiezan por su numero de registro
            if data[i][0].isdigit():
                # Eliminamos la K QUE SEPARA EL NUMERO DE REGISTRO CON EL RESTO
                numeros = re.findall("\A[0-9]* K", data[i])
                numero = numeros[0]
                # AL RESTO LE QUITAMOS CINTA Y OBTENEMOS SU TIPO DE CINTA
                cintas = re.findall("CINTA*.*", data[i])
                cinta = cintas[-1]
                # OBTENEMOS EL TIPO DEL PROGRAMA DEL RESTO
                tipos = self.__check_type_program(data[i])
                tipo = tipos[0]
                # LE QUITAMOS EL RESTO QUE NO NOS SIRVE
                nombre = tipos[1].replace(cinta, "")
                nombre = nombre.replace(numero, "")

                nombre = re.sub("\A\s", "", nombre)
                nombre = re.sub("\s\Z", "", nombre)

                numero = numero.replace("K", "")
                numero = numero.replace(" ", "")

                new_list_list.append(numero)
                new_list_list.append(nombre)
                new_list_list.append(tipo)

                cinta = cinta.replace("CINTA", "")
                cinta = cinta.replace(" ", "")
                new_list_list.append(cinta)
            i = i + 1
        return new_list_list

    def __hay_resultado(self, data):
        """Devuelve true en caso de que haya un programa con el nombre buscado"""
        hay_resultado = True
        i = 0
        substring = "NO HAY NINGUN POGRAMA CON ESE NOMBRE"
        # A veces no lee correctamente la pantalla, mi posiblemente haya que
        # cambiar el valor de estos strings
        subsubstring = "RR HAR RIRGUR PRRGRARA CRR ESE RRMRREE PULSA ERTER"

        while i < len(data) and hay_resultado:
            if substring in data[i] or subsubstring in data[i]:
                hay_resultado = False
            i = i + 1
        return hay_resultado

    def __quedan_resultados(self, data):
        """Verifica si hay alguna opcion mas disponible"""
        hay_resultado = False
        i = 0
        substring = "ES ESTE "
        while i < len(data) and not hay_resultado:
            if substring in data[i]:
                hay_resultado = True
            i = i + 1
        return hay_resultado

    def __preprocess_image(self):
        image = cv2.imread('images/captura.png')

        # Incrementamos el tamaño de la imagen para mejor procesado de pytesseract
        image = cv2.resize(image, (0, 0), fx=7, fy=7)

        # Cambiamos el espaio de colores a escala de grises. Necesario para threshold
        gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)

        # Invierte los colores a fondo blanco y letras negras. THRESH_OTSU deja las letras más legibles
        thresh = cv2.threshold(gray, 127, 255, cv2.THRESH_BINARY_INV + cv2.THRESH_OTSU)[1]
        cv2.imwrite("images/inverted.png", thresh)

        # Configuramos a pytesseract para que diferencie espacios de tabuladores y transformamos a string
        con = '-c preserve_interword_spaces=1 --psm 6'
        data = pytesseract.image_to_string('images/inverted.png', lang="eng", config=con)

        return data

    def __some_string_fixes(self, lista):
        # Limpia algunos caracteres mal leidos
        lista = lista.replace("> ", "S")
        lista = lista.replace("?", "7")
        lista = lista.replace("|", "I")
        lista = lista.replace("AC IONAL", "ACIONAL")
        lista = lista.replace("Q)", "Q")
        lista = lista.replace("». DEPOR", "S.DEPOR")
        lista = lista.replace("VIDEQA", "VIDEOA")
        lista = lista.replace("¢@", "7")
        lista = lista.replace("’ S", "’")
        lista = lista.replace("X<I", "XXI")
        return lista

    # Quita lineas vacias
    def __remove_empty_lines(self, lista):
        for i in lista:
            if i == "":
                lista.remove(i)
        return lista

    # Comprueba si está en la pantalla menu
    def __is_menu(self):
        lista = self.__preprocess_image()
        lista = lista.split('\n')
        self.__remove_empty_lines(lista)
        # Busco linea MENU
        if lista[0] == 'MENU':
            return True
        else:
            return False

    def __listado_texto_captura(self):
        # Hago otro pantallazo
        pantalla = self.screenshot_only()
        lista = self.__preprocess_image()

        # Separo por lineas
        lista = lista.split('\n')

        # Elimino las lineas vacias
        lista = self.__remove_empty_lines(lista)
        lAux = []
        for x in lista:
            # Sustituyo cuando hay dos o mas espacios seguidos por tabulador y arreglo los resultados
            aux = re.sub("\\s\s+", "\t", x)
            aux = self.__some_string_fixes(aux)

            # Separo por tabulador para obtener los elementos de una linea
            aux = aux.split("\t")

            # Compruebo que no es una linea basura
            isPulsa = aux[0] == 'PULSA SPACE PARA CONTINUAR U OTRA TECLA PARA ACABAR'
            isFin = aux[0] == '\x0c'
            if (not isPulsa) and (not isFin):
                isCabecera = (aux[1] == 'NOMBRE' and aux[2] == 'TIPO')
                if not isCabecera:
                    # Si es linea valida, la conservo
                    aux2 = aux[1]
                    # Arreglamos si alguna componente no tiene 4 elementos (por mala separacion por tabuladores
                    if len(aux2) > 30:
                        aux[1] = aux2[0:30]
                        aux3 = aux[2]
                        aux[2] = aux2[31:]
                        aux = aux + [aux3]
                    lAux = lAux + [aux]
        return lAux

    def __delete_last_first(self, lista):
        lAux = []
        i = 1
        for x in lista:
            # Elimino primero y ultimo
            aux = x[1:-1]
            # Añado numero como primer elemento
            aux = [str(i)] + aux
            lAux = lAux + [aux]
            i = i + 1
        return lAux

    def __listado_registros(self):
        time.sleep(0.25)
        lista = []
        pantalla = self.screenshot_only()

        # Compruebo que no estoy en la pantalla de menu
        ismenu = self.__is_menu()
        while not ismenu:
            lista2 = self.__listado_texto_captura()
            pyautogui.press('space')
            lista = lista + lista2
            pantalla = self.screenshot_only()
            ismenu = self.__is_menu()
        lista = self.__delete_last_first(lista)
        return lista

    def __ordenar_por_grabacion(self):
        pyautogui.press('3')
        pyautogui.press('enter')
        pyautogui.press('4')
        pyautogui.press('enter')
        pantalla = self.screenshot_only()
        isMenu = self.__is_menu()
        while not isMenu:
            pantalla = self.screenshot_only()
            isMenu = self.__is_menu()


    def __listar_todos_programas_cinta(self, cinta, lista):
        lAux=[]
        for i in lista:
            #Miramos si coincide la cinta con la buscada
            if i[3] == cinta:
                lAux=lAux+[i]
            else:
                #Buscamos tambien entre los programas en multiples cintas
                tape = i[3].split("-")
                found = True
                j = 0
                while found and j < len(tape):
                    if tape[j] == cinta:
                        # Coincide y debemos añadirlo a la lista
                        found=False
                        lAux=lAux+[i]
                    else:
                        j = j + 1
        return lAux


    ###########################################
    ##      FUNCIONES PUBLICAS              ##
    ###########################################

    """ BUSCAR DATOS SIN CONOCER EL NUMERO DE REGISTRO """

    def find_result(self, programa):
        """Dato del nombre de un programa busca si hay alguno con ese nombre """
        pyautogui.press('7')
        programa = programa.upper()
        pyautogui.press('N')
        pyautogui.press('enter')
        pyautogui.write(programa)
        pyautogui.press('enter')
        time.sleep(1.5)
        pantalla = self.screenshot()
        booleano = self.__hay_resultado(pantalla)

        if booleano == False:
            pyautogui.press('enter')
            pyautogui.press('N')
            pyautogui.press('enter')
            # Devuelve True o False si ha encontrado un programa o no con ese nombre
        return booleano

    def obtener_resultado(self):
        """ Si anteriormente ha encontrado un programa con ese nombre
            obtiene este programa y lo devuelve en formato de lista """
        pantalla = self.screenshot()
        pantalla = self.__clean_list(pantalla)
        resultado = self.__filter_result(pantalla)
        return resultado

    def confirmar_resultado(self):
        """ Confirma que ese el programa que queremos"""
        pyautogui.press('S')
        pyautogui.press('enter')
        pyautogui.press('N')
        pyautogui.press('enter')
        pyautogui.press('N')
        pyautogui.press('enter')
        # Al terminar nos devuelve al Menu principal

    def elegir_otro_resultado(self):
        """ Busca si existe otro resultado y si es asi lo devuelve"""
        pantalla = self.screenshot()
        resultado = []
        booleano = self.__quedan_resultados(pantalla)
        if booleano:
            pyautogui.press('N')
            pyautogui.press('enter')
            time.sleep(0.5)
            booleano = self.__hay_resultado(pantalla)
            if booleano:
                resultado = self.obtener_resultado()
            else:
                pyautogui.press('enter')
                pyautogui.press('N')
                pyautogui.press('enter')
        return resultado

    """ LISTADO DE TODOS LOS REGISTROS DEL SISTEMA
        DEVUELVE LISTA DE LISTAS:
        [[numero, nombre, tipo, cinta]] """

    def lista_todos_los_programas(self):
        #NOS ASEGURAMOS DE QUE VAMOS A LEER POR ORDEN DE GRABACION
        #self.__ordenar_por_grabacion()
        #VAMOS AL LISTADO
        pyautogui.press('6')
        pyautogui.press('enter')
        lista = self.__listado_registros()
        return lista


    def lista_programas_una_cinta(self,cinta):
        #obtenemos el listado de todos los programas
        lista=self.lista_todos_los_programas()
        #filtramos por cinta
        lista=self.__listar_todos_programas_cinta(cinta,lista)
        return lista




# Conseguir la pantalla de la aplicacion legada
#w = WindowMgr()
#w.start_project()
#time.sleep(5)

#lista = w.lista_programas_una_cinta("E")
#for i in lista:
#    print(i)

# print(w.screenshot())

# Hacer captura de la ventana de la aplicacion legada
# w.screenshot()

# Setup de tesseract


# Obtener texto de la captura
# print(pytesseract.image_to_string(Image.open('images/captura.png'), lang='spa'))

# mucho_texto=(pytesseract.image_to_string(Image.open('images/captura.png'), lang='spa'))
# print(len(mucho_texto))
# mucho_texto=mucho_texto.replace("\n","_")
# print(mucho_texto)
# w.find_result("CA")
# print(w.obtener_resultado())
# print(w.elegir_otro_resultado())
# print(w.elegir_otro_resultado())
# w.confirmar_resultado()

# print(w.find_result("BAD BUNNY"))
#
# w.obtener_resultado()
'''
w.find_result("CAR")
print(w.obtener_resultado())
print(w.elegir_otro_resultado())
print(w.elegir_otro_resultado())
w.confirmar_resultado()

w.find_result("TAR")
print(w.obtener_resultado())
print(w.elegir_otro_resultado())
print(w.elegir_otro_resultado())
w.confirmar_resultado()

w.find_result("DAR")
print(w.obtener_resultado())
print(w.elegir_otro_resultado())
print(w.elegir_otro_resultado())
w.confirmar_resultado()

w.find_result("BU")
print(w.obtener_resultado())
print(w.elegir_otro_resultado())
w.confirmar_resultado()

w.find_result("ZZZ")
'''
