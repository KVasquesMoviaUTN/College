#include <iostream>
#include <stdio.h>
#include <string.h>

using namespace std;

struct Estudiante{
    string nombre;
    string idioma;
    int nivel=0;
    int dni=0;
    int codigo=0;

    Estudiante *siguiente;
};

struct Docente{
    int dni=0;
    string nombre;
    int codigoDeCurso=0;
    int alumnos=0;
    Docente *der;
    Docente *izq;
};

struct Instituto{
    int codigoDeCurso=-1;
    char nombre[20];
    char idioma[20];//Candidato a eliminar
    int nivel=0;
    int cupo=0;
    int alumnos=0;
    int dni=0;
};

void CargarCodigos(string [], Instituto []);
bool BuscarCursoArch(int , string []);
void AgregarALaCola(Estudiante *&, int );
bool VerificarCupo(int , Instituto []);
void ArrayDocentes(Instituto [], string [], Instituto []);
void InscribirEstudiante(Estudiante *&, int); ///PUNTO 1
void EliminarEstudiante(Estudiante *&, int, Estudiante *&, Instituto []); ///PUNTO 2
int BuscarCurso(Estudiante *&lista, int);
void MostrarLista(Estudiante *, Instituto [], int);
void BorrarCola(Estudiante *&, int);
void Ordenar(Instituto []);
Docente *CrearDocente(Instituto [], int);
void InsertarDocente(Docente *&, Instituto [], int);
void InOrden(Docente *);
void ArchivosInscriptos(Estudiante *&, int);


int main(){
    int code = -1, dni = 0, ingreso=0, b=0;
    string nombre, idiomaEst;
    Instituto codigos[48];
    Instituto profesores[48];

    bool existente, error = false;
    Docente *docentes = NULL;
    Estudiante *alumnos = NULL;
    Estudiante *colaEspera = NULL;

    string idiomas[] = {"ingles.dat", "portugues.dat", "italiano.dat", "chino.dat", "aleman.dat", "frances.dat"};

    CargarCodigos(idiomas, codigos);

    while(true){
        if(!error) {
            cout << "Ingrese n para: \n\n1: Inscribir a un estudiante.\n2: Baja de estudiante.\n3: Mostrar inscripciones.\n4: Salir del programa.\n\n";
            cin >> ingreso;
        }

        error = false;

        if (ingreso == 1) {
            while (true) {
                cout << "Ingrese codigo de curso para inscribirse: ";
                cin >> code;

                existente = BuscarCursoArch(code, idiomas);
                if(!existente){
                    cout << "INSCRIPCION RECHAZADA\n\n";
                }else{
                    bool hayCupo;
                    hayCupo = VerificarCupo(code, codigos);
                    if(!hayCupo){
                        cout << "\n>> ISCRIBIENDO EN COLA DE ESPERA <<\n";
                        AgregarALaCola(colaEspera, code);
                    }else{
                        InscribirEstudiante(alumnos, code);

                        for(int i=0; i<48; i++){
                            if(codigos[i].codigoDeCurso == code){
                                codigos[i].cupo--;
                                codigos[i].alumnos++;
                                break;
                            }
                        }
                    }
                }
                break;
            }

        } else if (ingreso == 2) {
            cout << "Ingrese documento del alumno a eliminar de la lista: ";
            cin >> dni;

            EliminarEstudiante(alumnos, dni, colaEspera, codigos);

            cout << "Estudiante eliminado de la lista." << endl;

        } else if (ingreso == 3) {

            Ordenar(codigos);
            for (int i = 0; i < 48; i++) {
                if (codigos[i].codigoDeCurso != -1) {
                    cout << "Codigo de Curso: " << codigos[i].codigoDeCurso << endl;
                    cout << "Vacantes Ocupadas: " << codigos[i].alumnos << endl;
                    cout << "Vacantes Disponibles: " << codigos[i].cupo << endl;

                    cout << "\nLISTA ALUMNOS INSCRIPTOS:\n";
                    MostrarLista(alumnos, codigos, i);
                    cout << "----------------------------------------\n\n";
                }
            }
        } else if (ingreso == 4) {
            for (int i = 0; i < 48; i++) {
                if (codigos[i].codigoDeCurso != -1) {
                    cout << "COLA DE ESPERA CURSO " << " " << codigos[i].codigoDeCurso << endl;
                    MostrarLista(colaEspera, codigos, i);
                    cout << "----------------------------------------\n\n";
                }
            }
            ArrayDocentes(profesores, idiomas, codigos);

            while(profesores[b].codigoDeCurso != -1){
                InsertarDocente(docentes, profesores, b);
                b++;
            }

            for(int i=0; i<48; i++) {
                if(codigos[i].codigoDeCurso != -1)
                    ArchivosInscriptos(alumnos, codigos[i].codigoDeCurso);
            }

            cout << "ARBOL:\n";
            InOrden(docentes);

            cout << "\nFIN DEL PROGRAMA";
            break;

        } else {
            cout << "Ingrese un numero del 1 al 4";
            cin >> ingreso;
            error = true;
        }
    }
}

void CargarCodigos(string idioma[], Instituto codigos[]){
    Instituto a;
    int pos=0;

    for(int i=0; i<6; i++){
        FILE * f = fopen( idioma[i].c_str(), "rb");
        fread(&a, sizeof(Instituto), 1 , f);
        while (!feof (f)){
            codigos[pos].cupo = a.cupo;
            codigos[pos].codigoDeCurso = a.codigoDeCurso;

            pos++;
            fread(&a, sizeof(Instituto), 1 , f);
        }
        fclose(f);
    }
}

bool BuscarCursoArch(int code, string idioma[]){
    Instituto a;
    bool existe = false;

    for(int i=0; i<6; i++){
        FILE * f = fopen( idioma[i].c_str(), "r+b");
        fread(&a, sizeof(Instituto), 1 , f);
        while (!feof (f)){
            if(a.codigoDeCurso == code){
                existe = true;
            }
            fread(&a, sizeof(Instituto), 1 , f);
        }
        fclose(f);
    }
    return existe;
}

///////SOLO INGRESA AL ESTUDIANTE Y RESTA CUPO
void InscribirEstudiante(Estudiante *&alumnos, int code){
    Estudiante *lista = new Estudiante();
    int dni;

    Estudiante *aux = alumnos;
    Estudiante *aux2;

    cout << "Ingrese nombre del estudiante: ";
    cin >> lista->nombre;

    cout << "Ingrese DNI del estudiante: ";
    cin >> lista->dni;
    while(lista->dni < 1){
        cout << "Ingrese DNI nuevamente: ";
        cin >> lista->dni;
    }
    dni = lista->dni;

    cout << endl;

    lista->codigo = code;


    while((aux != NULL) && (aux->dni < dni)){
        aux2 = aux;
        aux = aux->siguiente;
    }

    if(alumnos == aux){
        alumnos = lista;
    }else{
        aux2->siguiente = lista;
    }

    lista->siguiente = aux;
    cout << "Inscripcion exitosa\n\n";
}

bool VerificarCupo(int code, Instituto array[]) {
    for (int i = 0; i < 48; i++) {
        if (array[i].codigoDeCurso == code) {
            if (array[i].cupo <= 0) {
                return false;
            } else{
                return true;
            }
        }
    }
    cout << "codigo de curso no coincide" << endl;//28/11
}

void AgregarALaCola(Estudiante *&colaEspera, int code) {
    Estudiante *cola = new Estudiante();

    Estudiante *aux = colaEspera;
    Estudiante *aux2;

    cout << "Ingrese nombre del estudiante: ";
    cin >> cola->nombre;

    cout << "Ingrese DNI del estudiante: ";
    cin >> cola->dni;
    while (cola->dni < 1) {
        cout << "Ingrese DNI nuevamente: ";
        cin >> cola->dni;
    }

    cout << endl;

    cola->codigo = code;


    while (aux != NULL) {
        aux2 = aux;
        aux = aux->siguiente;
    }

    if (colaEspera == aux) {
        colaEspera = cola;
    } else {
        aux2->siguiente = cola;
    }
    cola->siguiente = aux;
    cout << "\ninscripcion exitosa\n";
}

void EliminarEstudiante(Estudiante *&lista, int dni, Estudiante *&colaEspera, Instituto codigos[]){

    Estudiante *nuevoNodo = new Estudiante();
    Estudiante *agregar = new Estudiante();
    Estudiante *auxCola = colaEspera;
    int dniAux, codigo, pos;

    codigo = BuscarCurso(lista, dni);
    for(int i=0; i<48; i++){
        if(codigos[i].codigoDeCurso == codigo){
            pos = i;
            break;
        }
    }

    if(lista != NULL){
        Estudiante *anterior = NULL;
        Estudiante *aux_borrar = lista;

        while((aux_borrar != NULL) && (aux_borrar->dni != dni)){
            anterior = aux_borrar;
            aux_borrar = aux_borrar->siguiente;
        }

        if(aux_borrar == NULL){
            cout << "El elemento no existe";
        }else if(anterior == NULL){
            lista = lista->siguiente;
            delete aux_borrar;
            codigos[pos].cupo++;
            codigos[pos].alumnos--;
        }else{
            anterior->siguiente = aux_borrar->siguiente;
            delete aux_borrar;
            codigos[pos].cupo++;
            codigos[pos].alumnos--;
        }
    }

    Estudiante *aux = lista;
    Estudiante *aux2 = NULL;

    //PASAR DATOS
    while(auxCola != NULL){
        if(auxCola->codigo == codigo){
            nuevoNodo = auxCola;
            codigos[pos].cupo--;
            codigos[pos].alumnos++;
            break;
        }
        auxCola = auxCola->siguiente;
    }
    dniAux = nuevoNodo->dni;

    agregar->dni = nuevoNodo->dni;
    agregar->codigo = nuevoNodo->codigo;
    agregar->nombre = nuevoNodo->nombre;
    agregar->nivel = nuevoNodo->nivel;
    agregar->idioma = nuevoNodo->idioma;

    while((aux != NULL) && (aux->dni < dniAux)){
        aux2 = aux;
        aux = aux->siguiente;
    }

    if(lista == aux){
        lista = lista;
    }else{
        aux2->siguiente = agregar;
    }

    agregar->siguiente = aux;

    BorrarCola (colaEspera, dniAux);
}

int BuscarCurso(Estudiante *&lista, int dni){
    int codigo;
    Estudiante *aux = lista;

    while((aux != NULL) && (aux->dni != dni)){
        aux = aux->siguiente;
    }
    codigo = aux->codigo;
    return codigo;
}

void MostrarLista(Estudiante *alumnos, Instituto codigos[], int pos) {
    Estudiante *lista = alumnos;
    if (lista == NULL) {
        cout << "Soy una lista vacia :(\n" << endl;
    } else {
        while (lista != NULL) {
            if (lista->codigo == codigos[pos].codigoDeCurso) {
                cout << "Nombre: " << lista->nombre << endl;
                cout << "DNI: " << lista->dni << endl;
                cout << "\n";
            }
            lista = lista->siguiente;
        }
        lista = alumnos;
    }
}

void BorrarCola(Estudiante *&lista, int dni){

    if(lista != NULL){
        Estudiante *auxborrar;
        Estudiante *anterior = NULL;
        auxborrar = lista;
        while((auxborrar != NULL) && (auxborrar->dni != dni)){
            anterior = auxborrar;
            auxborrar = auxborrar->siguiente;
        }
        if(auxborrar == NULL){
            cout << "El elemento no existe.";
        }else if(anterior == NULL){
            lista = lista->siguiente;
            delete auxborrar;
        }else{
            anterior->siguiente = auxborrar->siguiente;
            delete auxborrar;
        }
    }
}

void Ordenar(Instituto codigo[]){ ///ORDENA LOS COGIGOS DE MENOR A MAYOR PARA MOSTRAR EL PUNTO 3
    Instituto aux;
    for(int i=0; i<48; i++){
        for(int j=0; j<47; j++){
            if(codigo[j].codigoDeCurso > codigo[j+1].codigoDeCurso){
                aux = codigo[j];
                codigo[j] = codigo[j+1];
                codigo[j+1] = aux;
            }
        }
    }
}

void ArrayDocentes(Instituto docente[], string idioma[], Instituto codigos[]){
    int pos=0;
    for(int k=0; k<6; k++) {
        FILE *arch = fopen(idioma[k].c_str(), "rb");
        fread(&docente[pos], sizeof(Instituto), 1, arch);
        while (!feof(arch)) {
            pos++;
            fread(&docente[pos], sizeof(Instituto), 1, arch);
        }
        fclose(arch);
    }

    for(int i=0; i<48; i++){
        for(int j=0; j<pos; j++){
            if(docente[j].codigoDeCurso == codigos[i].codigoDeCurso)
                docente[j].alumnos = codigos[i].alumnos;
        }
    }
}

Docente *CrearDocente(Instituto profes[], int pos){
    Docente *nuevoNodo = new Docente();

    nuevoNodo->dni = profes[pos].dni;
    nuevoNodo->nombre = profes[pos].nombre;
    nuevoNodo->codigoDeCurso = profes[pos].codigoDeCurso;
    nuevoNodo->alumnos = profes[pos].alumnos;
    nuevoNodo->der = NULL;
    nuevoNodo->izq = NULL;

    return nuevoNodo;
}

void InsertarDocente(Docente *&docentes, Instituto profes[], int pos){
    if(docentes == NULL){//ARBOL VACIO
        Docente *nuevoNodo = CrearDocente(profes, pos);
        docentes = nuevoNodo;
    }else{
        int valorRaiz = docentes->dni;//Valor de la raiz
        if(profes[pos].dni < valorRaiz){
            InsertarDocente(docentes->izq, profes, pos);

        }else{
            InsertarDocente(docentes->der, profes, pos);
        }
    }
}

void InOrden(Docente *docentes){
    if(docentes == NULL){
        return;
    }else{
        InOrden(docentes->izq);
        cout << "DNI-> " << docentes->dni << " - ";
        cout << "Curso-> " << docentes->codigoDeCurso << " - ";
        cout << "Nombre-> " << docentes->nombre << " - ";
        cout << "Alumnos inscriptos-> " << docentes->alumnos;
        cout << endl;
        InOrden(docentes->der);
    }
}

void ArchivosInscriptos(Estudiante *&inscriptos, int code){
    Estudiante *lista = inscriptos;
    int aux, i=0;
    int code2=code;
    char x[12], datAux[4] = {'.','d','a','t'};

    for(int a=0; a < size(x); a++){
        x[a] = 0;//Inicio a NULL
    }
    while(code2 > 0){
        aux = code2%10;//Guarda el ultimo digito de n en aux
        code2 /= 10;//Saca un digito a n
        x[i] = aux+48;//Guarda n+48 (valor de n en ASCII)
        i++;
    }
    strrev(x);//Invierto arreglo

    for(int j=0; j<4; j++){
        x[i+j] = datAux[j];
    }

    FILE *f = fopen(x, "wb");
    while(lista != NULL) {
        if(lista->codigo == code){
            fwrite(&lista, sizeof(Estudiante), 1, f);
        }
        lista = lista->siguiente;
    }
    lista = inscriptos;
    fclose(f);
}