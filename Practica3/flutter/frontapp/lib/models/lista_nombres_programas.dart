import 'dart:collection';

import 'package:flutter/material.dart';

class ListaNombresProgramas extends ChangeNotifier {
  List<String> _listaNombres = [];

  UnmodifiableListView<String> get listaNombres =>
      UnmodifiableListView(_listaNombres);

  int get numPrograms => _listaNombres.length;

  set listaNombres(List<String> listaNombres) {
    _listaNombres = listaNombres;
    notifyListeners();
  }
}
