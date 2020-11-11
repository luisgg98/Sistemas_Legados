from django.apps import AppConfig

class MypageConfig(AppConfig):
    name = 'mypage'
    def ready(self):
        pass #singleton.start_project()



