from py3270 import Emulator
import time

#Espera activa para que pantalla este preparada
def __waitScreen():
    while em.string_get(1,2,1) == " ":
        pass


#IMPORTANTE: HACER ESTO EN UNA FUNCION, ES UNA API

#def iniciar():
    #todo lo de abajo

#em = Emulator()
em = Emulator(visible=True)
em.connect('155.210.152.51:104')
em.send_enter()

__waitScreen()
print(em.string_get(3,1,17))
em.fill_field(3,18,'grupo_04',8)
em.fill_field(5,18,'secreto6',8)
em.send_enter()
__waitScreen()
print(em.string_get(41,1,17))
em.send_enter()
while em.string_get(1,2,1) == "U":
        pass

em.save_screen("/home/irene/Documentos/legados/p2/file.html")
print(em.string_get(1,1,17))
print(em.string_get(3,1,13))
em.fill_field(3,15,'tareas.c',8)
em.send_enter()
__waitScreen()
#Tenemos que esperar a que cargue todo lo de ejecutar tareas.c -> (sleep?)
time.sleep(2)
em.save_screen("/home/irene/Documentos/legados/p2/file.html")
em.terminate()
