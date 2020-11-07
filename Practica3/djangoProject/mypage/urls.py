from django.urls import path
from . import views, prueba
from django.http import HttpResponse
urlpatterns = [
    path('find_by_name/', prueba.find_by_name),
]