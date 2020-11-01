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
    """FUNCIONES PRIVADAS"""
    def __init__(self):
        """Constructor"""
        self._handle = None


    def start_project(self):
        self.find_window_wildcard('.*DOSBox.*')
        win32gui.SetForegroundWindow(self._handle)
        pyautogui.press('enter') 


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
            if datas[i] != '' and datas[i]!= ' ':
                new_list.append(datas[i])
            else:
                pass
            i=i+1
        return new_list

    def __filter_result(self,data):
        i= 0
        new_list_list=""
        while i < len(data):
            if data[i][0].isdigit() :
                new_list_list=data[i]
            i=i+1
        return new_list_list


    def __hay_resultado(self,data):
        hay_resultado=True
        i= 0
        substring = "NO HAY NINGUN POGRAMA CON ESE NOMBRE"
        subsubstring ="RR HAR RIRGUR PRRGRARA CRR ESE RRMRREE PULSA ERTER"

        while i < len(data) and hay_resultado:
            if substring in data[i] or subsubstring in data[i]:
                hay_resultado= False
            i=i+1
        return hay_resultado
    
    def __quedan_resultados(self,data):
        hay_resultado=False
        i= 0
        substring = "ES ESTE 7 "
        while i < len(data) and not hay_resultado:
            if substring in data[i]:
                hay_resultado= True
            i=i+1
        return hay_resultado       


    """ BUSCAR DATOS SIN CONOCER EL NUMERO DE REGISTRO """
    def find_result(self,programa):
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

        return booleano

    def obtener_resultado(self):
        pantalla = self.screenshot()
        pantalla= self.__clean_list(pantalla)
        resultado= self.__filter_result(pantalla)
        return resultado
            
    def confirmar_resultado(self):
        pyautogui.press('S')
        pyautogui.press('enter')
        pyautogui.press('N')
        pyautogui.press('enter')
        pyautogui.press('N')
        pyautogui.press('enter') 
 


    def elegir_otro_resultado(self):
        pantalla = self.screenshot()
        resultado=""
        booleano =self.__quedan_resultados(pantalla)
        if booleano:
            pyautogui.press('N')
            pyautogui.press('enter')
            time.sleep(0.2)
            booleano =self.__hay_resultado(pantalla)
            if booleano:
                resultado = self.obtener_resultado()
            else:
                pyautogui.press('enter')
                pyautogui.press('N')
                pyautogui.press('enter') 

        
        return resultado







        #  Comprobar que haya un buen resultado






# Conseguir la pantalla de la aplicacion legada
w = WindowMgr()
w.start_project()

# Hacer captura de la ventana de la aplicacion legada
#w.screenshot()

# Setup de tesseract
pytesseract.pytesseract.tesseract_cmd = r'C:\Program Files\Tesseract-OCR\tesseract'

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

print(w.find_result("BAD BUNNY"))
#
w.obtener_resultado()

#w.find_result("CAR")
#print(w.obtener_resultado())
#print(w.elegir_otro_resultado())
#print(w.elegir_otro_resultado())
#w.confirmar_resultado()

