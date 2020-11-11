from django.http import JsonResponse, HttpResponse
from wrapper import *

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


def descompose_program(resultado):
    data = {
        'registro': resultado[0],
        'nombre': resultado[1],
        'tipo': resultado[2],
        'cinta': resultado[3]

    }
    return data


def clean_result(lista):
    jsonlist = []
    for i in lista:
        jsonlist.append(descompose_program(i))

    return jsonlist

def find_by_name(request):
    if request.method == 'GET' :
        if request.GET.get('program') !='' and request.GET.get('program') != None:
            singleton = WindowMgr()
            programa = request.GET.get('program')
            resultado = singleton.find_program_by_name(programa)
            if resultado != []:
                return return_program(resultado)
            else:
                return no_result_found()
        else:
            return complain_about_program()
    else:
        return complain_about_get()

def accept_result(request):
    if request.method == 'GET' :
        singleton = WindowMgr()
        singleton.start_project()
        singleton.confirmar_resultado()
        data = {
            'message': "Se ha confirmado el resultado",

        }
        return JsonResponse(data)
    else:
        return complain_about_get()

def choose_another(request):
    if request.method == 'GET':
        singleton = WindowMgr()
        singleton.start_project()
        resultado = singleton.elegir_otro_resultado()
        if resultado != []:
            return return_program(resultado)
        else:
            return no_result_found()
    else:
        return complain_about_get()

def get_them_all(request):
    singleton = WindowMgr()
    programa =singleton.lista_todos_los_programas()
    data = {
        'data':clean_result(programa),
    }
    return JsonResponse(data)

def get_tape_all(request):
    if request.method == 'GET':
        if request.GET.get('cinta') != '' and request.GET.get('cinta') != None:
            singleton = WindowMgr()
            cinta = request.GET.get('cinta')
            resultado = singleton.lista_programas_una_cinta(cinta)
            if resultado != []:
                data = {
                    'data': clean_result(resultado),

                }

                return JsonResponse(data)

            else:
                return no_result_found()

        else:
            return complain_about_program()

    else:
        return complain_about_get()




# Create your views here.
