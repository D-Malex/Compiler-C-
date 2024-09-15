#include <iostream>
#include <cmath>

using namespace std;

double raiz_cuadrada_de_positivo(double x) {
    if (x == 1.0) {
        return 1.0;
    }

    double izq;
    double der;

    if (x < 1.0) {
        izq = x;
        der = 1.0;
    } else {
        izq = 1.0;
        der = x;
    }

    double r = (izq + der) / 2.0;

    while (abs(x - r * r) > 0.0000000001) {
        if (r * r < x) {
            izq = r;
        } else {
            der = r;
        }

        r = (izq + der) / 2.0;
    }

    return r;
}

int main() {
    cout << "***************************************************************\n";
    cout << "Se ingresa un valor numerico, se muestran sus raices cuadradas.\n";
    cout << "Se utiliza el algoritmo de la biseccion.\n";
    cout << "***************************************************************\n";

    string renglon;

    cout << "x: ";
    getline (cin, renglon);
    double x = stof(renglon);


    if (x == 0.0) {
        cout << "La raiz cuadrada de 0 es 0.00000000\n";
    } else {
        double rc = raiz_cuadrada_de_positivo(abs(x));

        if (x < 0.0) {
            if (x == -1.0) {
                cout << "Las raices cuadradas de -1 son +i y -i\n";
            } else {
                cout << 
                    "Las raices cuadradas de " << x <<
                    " son +" << rc << "i y -" << rc << "i\n";
            }
        } else {
            cout << "Las raices cuadradas de " << x <<
                    " son +" << rc << " y -" << rc << "\n";
        }
    }

    return 0;
}
