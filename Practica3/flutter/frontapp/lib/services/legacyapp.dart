import 'package:frontapp/models/program.dart';
import 'package:http/http.dart' as http;
import 'dart:convert';

Future<List<Program>> fetchPrograms() async {
  final response =
      await http.get('https://jsonplaceholder.typicode.com/albums/1');

  if (response.statusCode == 200) {
    // If the server did return a 200 OK response,
    // then parse the JSON.
    List programas = jsonDecode(response.body)['data'];
    List<Program> listaProgramas = [];
    programas.forEach((element) {
      listaProgramas.add(Program(
          registro: element['registro'],
          nombre: element['nombre'],
          tipo: element['tipo'],
          cinta: element['cinta']));
    });
  } else {
    // If the server did not return a 200 OK response,
    // then throw an exception.
    throw Exception('Failed to load album');
  }
}
