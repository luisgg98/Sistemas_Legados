class Program {
  String registro;
  String nombre;
  String tipo;
  String cinta;

  Program({this.registro, this.nombre, this.tipo, this.cinta});

  Program.fromJson(Map<String, dynamic> json) {
    registro = json['registro'];
    nombre = json['nombre'];
    tipo = json['tipo'];
    cinta = json['cinta'];
  }
}
