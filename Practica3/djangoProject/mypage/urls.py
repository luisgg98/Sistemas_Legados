from django.urls import path
from . import views
from django.http import HttpResponse
#get_them_all
urlpatterns = [
    path('find_by_name/', views.find_by_name),
    path('accept_result/', views.accept_result),
    path('choose_another/', views.choose_another),
    path('get_them_all/', views.get_them_all),
    path('get_tape_all/', views.get_tape_all),
]