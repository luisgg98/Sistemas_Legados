import pyautogui
import pytesseract
from PIL import Image
from PIL import ImageGrab
import win32gui
import win32con
import re
import time

class WindowMgr:
    """Permite encontrar una ventana y almacenarla para hacer screenshot"""

    ###########################################
    ##      FUNCIONES PRIVADAS               ##
    ###########################################

    """FUNCIONES PRIVADAS"""
    def __init__(self):
        """Constructor"""
        self._handle = None

    def __check_type_program(self,programa):
        """Verifica cual es el tipo del programa"""
        tipo_encontrado="Indeterminado"
        #Ya que el numero de tipos es finito, compruebo manualmente
        #a cual se corresponde, lo elimino de la descripcion del programa
        #y devuelvo ambos
        x = re.search("\sVIDE[A-Z]*A\s", programa)
        if x:
            programa= re.sub("\sVIDE[A-Z]*A\s","",programa)
            return ["VIDEOAVENTURA",programa]

        x = re.search("\sSIM[A-Z]*R\s", programa)
        if x:
            programa= re.sub("\sSIM[A-Z]*R\s","",programa)
            return ["SIMULADOR",programa]

        x = re.search("\sARCADE\s", programa)    
        if x:
            
            programa= re.sub("\sARCADE\s","",programa)
            return ["ARCADE",programa]
        
        x = re.search("\sCONV[A-Z]*\s", programa)    
        if x:
            programa= re.sub("\sCONV[A-Z]*\s","",programa)
            return ["CONVERSACIONAL",programa]

        x = re.search("\sJUEGO DE MESA\s", programa)    
        if x:
            programa= re.sub("\sJUEGO DE MESA\s","",programa)
            return ["JUEGO DE MESA",programa]

        x = re.search("\sS[A-Z]* D[A-Z]*TIVO\s", programa)    
        if x:
            programa= re.sub("\sS[A-Z]* D[A-Z]*TIVO\s","",programa)
            return ["S. DEPORTIVO" ,programa]       

        x = re.search("\sESTR[A-Z]*A\s", programa)    
        if x:
            programa= re.sub("\sESTR[A-Z]*A\s","",programa)
            return ["ESTRATEGIA" ,programa] 
        
        return [tipo_encontrado, programa]

    def start_project(self):
        """Funcion para centrarse en la aplicacion legada """
        self.find_window_wildcard('.*DOSBox.*')
        win32gui.SetForegroundWindow(self._handle)
        #Es necesario para forzarlo a centrarse en la aplicacion
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
        #win32gui.SetForegroundWindow(self._handle)
        x, y, x1, y1 = win32gui.GetClientRect(self._handle)
        x, y = win32gui.ClientToScreen(self._handle, (x, y))
        x1, y1 = win32gui.ClientToScreen(self._handle, (x1 - x, y1 - y))
        pyautogui.screenshot('images/captura.png', region=(x, y, x1, y1))
        mucho_texto=(pytesseract.image_to_string(Image.open('images/captura.png'), lang='spa'))
        return (mucho_texto.split('\n'))


    def __clean_list(self,datas):
        """Elimina los elementos innecesarios de la lista"""
        i=0
        new_list=[]
        while i < (len(datas)-1):
            #Elimino las lineas de la pantalla que son o un espacio o cadena vacia
            if datas[i] != '' and datas[i]!= ' ':
                new_list.append(datas[i])
            else:
                pass
            #me quedo solo con el texto que pueda ser util
            i=i+1
        return new_list

    #Funcion utilizada para buscar un programa por su nombre
    def __filter_result(self,data):
        """ Data una lista de elementos se queda con el que empieza por un digito"""
        i= 0
        new_list_list=[]
        while i < len(data):
            #Esto es debido a que los elementos de los programas cuando los buscas por 
            #nombre te los muestra 1 por 1 y todos empiezan por su numero de registro
            if data[i][0].isdigit() :
                #Eliminamos la K QUE SEPARA EL NUMERO DE REGISTRO CON EL RESTO
                numeros = re.findall("\A[0-9]* K", data[i])
                numero=numeros[0]
                #AL RESTO LE QUITAMOS CINTA Y OBTENEMOS SU TIPO DE CINTA
                cintas = re.findall("CINTA*.*",data[i])
                cinta=cintas[-1]
                #OBTENEMOS EL TIPO DEL PROGRAMA DEL RESTO 
                tipos = self.__check_type_program(data[i])
                tipo=tipos[0]
                #LE QUITAMOS EL RESTO QUE NO NOS SIRVE
                nombre=tipos[1].replace(cinta,"")
                nombre=nombre.replace(numero,"")

                nombre= re.sub("\A\s","",nombre)
                nombre= re.sub("\s\Z","",nombre)

                numero=numero.replace("K","")
                numero=numero.replace(" ","")

                new_list_list.append(numero)
                new_list_list.append(nombre)
                new_list_list.append(tipo)

                cinta=cinta.replace("CINTA","")
                cinta=cinta.replace(" ","")
                new_list_list.append(cinta)
            i=i+1
        return new_list_list


    def __hay_resultado(self,data):
        """Devuelve true en caso de que haya un programa con el nombre buscado"""
        hay_resultado=True
        i= 0
        substring = "NO HAY NINGUN POGRAMA CON ESE NOMBRE"
        #A veces no lee correctamente la pantalla, mi posiblemente haya que 
        #cambiar el valor de estos strings
        subsubstring ="RR HAR RIRGUR PRRGRARA CRR ESE RRMRREE PULSA ERTER"

        while i < len(data) and hay_resultado:
            if substring in data[i] or subsubstring in data[i]:
                hay_resultado= False
            i=i+1
        return hay_resultado
    

    def __quedan_resultados(self,data):
        """Verifica si hay alguna opcion mas disponible"""
        hay_resultado=False
        i= 0
        substring = "ES ESTE 7 "
        while i < len(data) and not hay_resultado:
            if substring in data[i]:
                hay_resultado= True
            i=i+1
        return hay_resultado       

    ###########################################
    ##      FUNCIONES PUBLICAS              ##
    ###########################################

    """ BUSCAR DATOS SIN CONOCER EL NUMERO DE REGISTRO """
    def find_result(self,programa):
        """Dato del nombre de un programa busca si hay alguno con ese nombre """
        pyautogui.press('7')
        programa=programa.upper()  
        pyautogui.press('N')
        pyautogui.press('enter') 
        pyautogui.write(programa)
        pyautogui.press('enter')
        time.sleep(1.5)
        pantalla = self.screenshot()
        booleano =self.__hay_resultado(pantalla)

        if booleano == False:
            pyautogui.press('enter')
            pyautogui.press('N')
            pyautogui.press('enter') 
        #Devuelve True o False si ha encontrado un programa o no con ese nombre
        return booleano

    def obtener_resultado(self):
        """ Si anteriormente ha encontrado un programa con ese nombre
            obtiene este programa y lo devuelve en formato de lista """
        pantalla = self.screenshot()
        pantalla= self.__clean_list(pantalla)
        resultado= self.__filter_result(pantalla)
        return resultado
            
    def confirmar_resultado(self):
        """ Confirma que ese el programa que queremos"""
        pyautogui.press('S')
        pyautogui.press('enter')
        pyautogui.press('N')
        pyautogui.press('enter')
        pyautogui.press('N')
        pyautogui.press('enter')
        #Al terminar nos devuelve al Menu principal 
 
    def elegir_otro_resultado(self):
        """ Busca si existe otro resultado y si es asi lo devuelve"""
        pantalla = self.screenshot()
        resultado=[]
        booleano =self.__quedan_resultados(pantalla)
        if booleano:
            pyautogui.press('N')
            pyautogui.press('enter')
            time.sleep(0.5)
            booleano =self.__hay_resultado(pantalla)
            if booleano:
                resultado = self.obtener_resultado()
            else:
                pyautogui.press('enter')
                pyautogui.press('N')
                pyautogui.press('enter')         
        return resultado


# Conseguir la pantalla de la aplicacion legada
w = WindowMgr()
w.start_project()
#print(w.screenshot())

# Hacer captura de la ventana de la aplicacion legada
#w.screenshot()

# Setup de tesseract


# Obtener texto de la captura
#print(pytesseract.image_to_string(Image.open('images/captura.png'), lang='spa'))

#mucho_texto=(pytesseract.image_to_string(Image.open('images/captura.png'), lang='spa'))
#print(len(mucho_texto))
#mucho_texto=mucho_texto.replace("\n","_")
#print(mucho_texto)
#w.find_result("CA")
#print(w.obtener_resultado())
#print(w.elegir_otro_resultado())
#print(w.elegir_otro_resultado())
#w.confirmar_resultado()

#print(w.find_result("BAD BUNNY"))
#
#w.obtener_resultado()

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
