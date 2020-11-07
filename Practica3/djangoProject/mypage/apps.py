from django.apps import AppConfig
from mypage.wrapper import *

class MypageConfig(AppConfig):
    name = 'mypage'
    def ready(self):
        pass #singleton.start_project()



