from django.urls import path
from . import views

urlpatterns = [
    path('find_by_name/', views.find_by_name),
    path('get_them_all/', views.get_them_all),
    path('get_tape_all/', views.get_tape_all),
]