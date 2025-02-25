from django.http import JsonResponse, HttpResponse
from wrapper import *

#####################################
#### FUNCIONES AUXILIARES       ####
#####################################

def complain_about_get():
    data = {
        'message': "La peticion tiene que ser GET",  
    }
    return JsonResponse(data,safe=False)

def complain_about_program():
    data = {
        'message': "No puedo entender el nombre el programa",  
    }
    return JsonResponse(data,safe=False)

def no_result_found():
    data = {
        'message': "No se han encontrado resultados",  
    }
    return JsonResponse(data,safe=False)


def return_program(resultado):
    print(resultado)
    data = {
        'registro': resultado[3],
        'nombre': resultado[0],
        'tipo': resultado[1],
        'cinta': resultado[2]

    }
    return JsonResponse(data,safe=False)


def descompose_program(resultado):
    data = {
        'registro': resultado[3],
        'nombre': resultado[0],
        'tipo': resultado[1],
        'cinta': resultado[2]

    }
    return data


def clean_result(lista):
    jsonlist = []
    for i in lista:
        jsonlist.append(descompose_program(i))

    return jsonlist


def form_list_names(lista):
    list_json = []
    for program in lista:
        data={
            'nombre':program
        }
        list_json.append(data)
    return list_json



#####################################
#### FUNCIONES PRINCIPALES       ####
#####################################


""" Find a program by its name"""
def find_by_name(request):
    if request.method == 'GET' :
        if request.GET.get('program') != '' and request.GET.get('program') != None:
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

"""Return a list with all the names of the programs"""
def get_them_all(request):
    singleton = WindowMgr()
    # De aqui se obtienen todos los programas
    programa =singleton.get_name_programs()
    data = form_list_names(programa)
    return JsonResponse(data,safe=False)

"""Return a list with all the programs on a tape"""
def get_tape_all(request):
    if request.method == 'GET':
        if request.GET.get('cinta') != '' and request.GET.get('cinta') != None:
            singleton = WindowMgr()
            cinta = request.GET.get('cinta')
            #De aqui se obtienen las cintas
            resultado = singleton.get_all_programs_tape(cinta)
            if resultado != []:
                data = clean_result(resultado)

                return JsonResponse(data,safe=False)

            else:
                return no_result_found()

        else:
            return complain_about_program()

    else:
        return complain_about_get()
