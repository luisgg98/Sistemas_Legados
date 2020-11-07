from django.http import JsonResponse
from mypage.wrapper import *

def complain_about_get():
    data = {
        'message': "La peticion tiene que ser GET",  # username of current logged-in user, otherwise Anonymous
    }
    return JsonResponse(data)

def complain_about_program():
    data = {
        'message': "No puedo entender el nombre el programa",  # username of current logged-in user, otherwise Anonymous
    }
    return JsonResponse(data)

def no_result_found():
    data = {
        'message': "No se han encontrado resultados",  # username of current logged-in user, otherwise Anonymous
    }
    return JsonResponse(data)

def return_program(resultado):
    data = {
        'registro': resultado[0],  # username of current logged-in user, otherwise Anonymous
        'nombre': resultado[1],
        'tipo': resultado[2],
        'cinta': resultado[3]

    }
    return JsonResponse(data)

def find_by_name(request):
    if request.method == 'GET' :
        if request.GET.get('program') !='' and request.GET.get('program') != None:
            singleton = WindowMgr()
            singleton.start_project()
            programa = request.GET.get('name')
            booleano = singleton.find_result(programa)
            if booleano == True:
                resultado = singleton.obtener_resultado()
                if resultado == []:
                    return_program(resultado)
                else:
                    no_result_found()

                return_program(resultado)
            else:
                no_result_found()

        else:
            complain_about_program()
    else:
        complain_about_get()

def accept_result(request):
    if request.method == 'GET' :
        singleton = WindowMgr()
        singleton.start_project()
        singleton.confirmar_resultado()
        data = {
            'message': "Se ha confirmado el resultado",  # username of current logged-in user, otherwise Anonymous

        }
        return JsonResponse(data)
    else:
        complain_about_get()

def choose_another(request):
    if request.method == 'GET':
        singleton = WindowMgr()
        singleton.start_project()
        resultado = singleton.elegir_otro_resultado()
        if resultado == []:
            return_program(resultado)
        else:
            no_result_found()
    else:
        complain_about_get()

def get_them_all(request):
    singleton = WindowMgr()
    singleton.start_project()
    programa =singleton.listado_programas()
    data = {
        'data': programa,  # username of current logged-in user, otherwise Anonymous

    }
    return JsonResponse(data)
# Create your views here.
