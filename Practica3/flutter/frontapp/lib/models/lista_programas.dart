import 'dart:collection';

import 'package:flutter/material.dart';
import 'package:frontapp/models/program.dart';

class ListaProgramas extends ChangeNotifier {
  List<Program> _listaProgramas = [];

  UnmodifiableListView<Program> get listaProgramas =>
      UnmodifiableListView(_listaProgramas);

  set listaProgramas(List<Program> listaProgramas) {
    _listaProgramas = listaProgramas;
    notifyListeners();
  }

  void deleteAll() {
    _listaProgramas.clear();
    notifyListeners();
  }
}
