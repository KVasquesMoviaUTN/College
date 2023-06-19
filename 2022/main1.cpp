#include <iostream>
#include <cstdio>

using namespace std;

struct Instituto{
    int codigoDeCurso=-1;
    char nombre[20];
    char idioma[20];//Candidato a eliminar
    int nivel=0;
    int cupo=0;
    int alumnos=0;
    int dni=0;
};

void CargarDatos(Instituto[], int[]);
void Burbujeo(Instituto[], int[], int);
void EscribirArchivos (Instituto[], int[], int, string);//PUNTO 2
void Informar(int [], string [], int []);
void CrearArchivos (int, string[]);//PUNTO 1
void Informar2(int [], string[]);

FILE *f;

int main(){
    int NIdioma = 0;
    string nombre;

    int contador[] = {0, 0, 0, 0, 0, 0};//IDIOMAS
    int contador2[] = {0, 0, 0, 0, 0, 0, 0, 0};//NIVELES

    Instituto ingles[8];
    Instituto portugues[8];
    Instituto aleman[8];
    Instituto frances[8];
    Instituto chino[8];
    Instituto italiano[8];

    string idiomas[] = {"Ingles", "Portugues", "Italiano", "Chino", "Aleman", "Frances"};
    string idiomas2[] = {"ingles", "portugues", "italiano", "chino", "aleman", "frances"};
    string idiomas3[] = {"INGLES", "PORTUGUES", "ITALIANO", "CHINO", "ALEMAN", "FRANCES"};

    string idiomaIngresado;

    cout << "Si quiere finalizar el ingreso de datos, presione x\n\nIngrese el idioma que desee, sin tilde por favor: ";
    cin >> idiomaIngresado;

    do {
        int cont = 0;
        for (int i = 0; i < 6; i++) {
            if (idiomaIngresado != idiomas[i] && idiomaIngresado != idiomas2[i] && idiomaIngresado != idiomas3[i]) {
                cont++;
            }
        }
        if (cont == 6) {
            cout << "Ingrese un idioma valido, por favor: ";
            cin >> idiomaIngresado;
        }

        for (int ac = 0; ac < 6; ac++) {//CREAR ARCHIVOS
            CrearArchivos(ac, idiomas);
        }

        for (int i = 0; i < 6; i++) {//Selecciono idioma
            if (idiomaIngresado == idiomas[i] || idiomaIngresado == idiomas2[i] || idiomaIngresado == idiomas3[i]) {
                NIdioma = i;
                contador[i]++;
            }
        }

//Asocio el nivel al parametro y posicion del array de ese idioma
        switch (NIdioma) {
            case 0:
                CargarDatos(ingles, contador2);
                break;

            case 1:
                CargarDatos(portugues, contador2);
                break;

            case 2:
                CargarDatos(italiano, contador2);
                break;

            case 3:
                CargarDatos(chino, contador2);
                break;

            case 4:
                CargarDatos(aleman, contador2);
                break;

            case 5:
                CargarDatos(frances, contador2);
                break;
        }
        cout << "Ingrese el idioma que desee, sin tilde por favor: ";
        cin >> idiomaIngresado;

    } while (idiomaIngresado != "x" && idiomaIngresado != "X");

    ///ORDENA POR CODIGO CADA IDIOMA
    Burbujeo(ingles, contador, 0);
    Burbujeo(portugues, contador, 1);
    Burbujeo(aleman, contador, 2);
    Burbujeo(frances, contador, 3);
    Burbujeo(chino, contador, 4);
    Burbujeo(italiano, contador, 5);

    ///ESCRIBE LOS ARCHIVOS CON LOS IDIOMAS ORDENADOS POR CODIGO
    EscribirArchivos(ingles, contador, 0, "ingles");
    EscribirArchivos(portugues, contador, 1, "portugues");
    EscribirArchivos(italiano, contador, 2, "italiano");
    EscribirArchivos(chino, contador, 3, "chino");
    EscribirArchivos(aleman, contador, 4, "aleman");
    EscribirArchivos(frances, contador, 5, "frances");

    Informar(contador, idiomas, contador2);
    Informar2(contador, idiomas);

    //////////LEER
    Instituto leerr;
    FILE * archivo = fopen( "ingles.dat", "rb");
    fread(&leerr, sizeof(Instituto), 1, archivo);
    while(!feof (archivo)){
        fread(&leerr, sizeof(Instituto), 1, archivo);
    }
    fclose(archivo);

    return 0;
}

void CargarDatos(Instituto idioma[], int contador2[]){
    int ingresoNivel;

    cout << "Ingrese el nivel: "; //Ingreso de datos por consola
    cin >> ingresoNivel;
    while(ingresoNivel < 0 || ingresoNivel > 9){
        cout << "Por favor, ingrese un numero valido del 1 al 8 :)\n";
        cin >> ingresoNivel;
    }
    contador2[ingresoNivel-1]++;

    while(idioma[ingresoNivel-1].codigoDeCurso != -1){
        cout << "Curso existente, ingrese otro: ";
        cin >> ingresoNivel;
    }
    cout << "Ingrese codigo de curso: ";
    cin >> idioma[ingresoNivel-1].codigoDeCurso;

    while(idioma[ingresoNivel-1].codigoDeCurso < 1){
        cout << "Codigo incorrecto, ingrese codigo de curso nuevamente: ";
        cin >> idioma[ingresoNivel-1].codigoDeCurso;
    }

    cout << "Ingrese cupo del curso: ";
    cin >> idioma[ingresoNivel-1].cupo;

    while(idioma[ingresoNivel-1].cupo < 1){
        cout << "Ingrese cupo de curso nuevamente: ";
        cin >> idioma[ingresoNivel-1].cupo;
    }
    cout << "Ingrese nombre del docente a cargo: ";
    cin >> idioma[ingresoNivel-1].nombre;

    cout << "Ingrese documento del docente a cargo: ";
    cin >> idioma[ingresoNivel-1].dni;
    if(idioma[ingresoNivel-1].dni < 1){
        cout << "Ingrese documento del docente a cargo: ";
        cin >> idioma[ingresoNivel-1].dni;
    }

    cout << "\n------------------------------------------------------------\n" << endl;

    idioma[ingresoNivel-1].nivel = ingresoNivel;
}

void Burbujeo(Instituto variable[], int contadorB[], int posicionB){
    Instituto aux, aux2;
    for(int i=0; i<8 ;i++){
        for(int j=7; j>0 ;j--){
            if((variable[j].codigoDeCurso > variable[j-1].codigoDeCurso)){
                aux = variable[j];
                variable[j] = variable[j-1];
                variable[j-1] = aux;
            }
        }
    }
    for(int k=0; k<contadorB[posicionB]; k++){
        for(int m=0; m<(contadorB[posicionB]-1); m++){
            if(variable[m].codigoDeCurso > variable[m+1].codigoDeCurso){
                aux2 = variable[m];
                variable[m] = variable[m+1];
                variable[m+1] = aux2;
            }
        }
    }
}

void EscribirArchivos(Instituto v[] , int contadorF[], int posicionF, string nombreArch){
    string nombre = nombreArch + ".dat";
    f = fopen(nombre.c_str(), "wb");

    for(int aa=0 ; aa < contadorF[posicionF] ; aa++){//
        fwrite(&v[aa], sizeof(Instituto), 1, f);
    }
    fclose(f);
}

void Informar2(int contadorF[], string idioma[]){
    for(int i=0; i<6; i++){
        cout << "Cursos de " << idioma[i] << ": " << contadorF[i] << endl;
    }
}

void Informar(int contadorI[], string idiomasI[], int contador2I[]){
    int ex=0, ex2=0;

    cout << "Idiomas con cursos en todos los niveles:\n";

    for(int info=0; info<6 ; info++) {
        if(contadorI[info] == 8){
            cout << idiomasI[info] << endl;
            ex++;
        }
    }
    if(ex == 0){
        cout << "\nNINGUNO\n";
    }

    cout << "\n\nNiveles sin idiomas:\n";

    for(int info2=0; info2<8; info2++){
        if(contador2I[info2] == 0){
            cout << info2 + 1 << endl;
            ex2++;
        }
    }
    if(ex2 == 0){
        cout << "NINGUNO\n ";
    }
}

void CrearArchivos (int n, string x[]){
    string w[6];
    w[n] = x[n] + ".dat";
    f = fopen(w[n].c_str(), "wb");

    fclose(f);
}