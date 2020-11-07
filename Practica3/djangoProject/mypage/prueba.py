from django.http import HttpResponse
from mypage.wrapper import *


def find_by_name(request):
    singleton = WindowMgr()
    singleton.start_project()
    programa = singleton.listado_programas()
    dato= len(programa)
    return HttpResponse(str(dato))
