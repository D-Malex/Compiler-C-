#include <iostream>

using namespace std;

int main() {
    cout << "************************************************************\n";
    cout << "Se ingresan dos valores enteros, se muestra su producto.\n";
    cout << "Se utiliza el algoritmo de 'multiplicacion por duplicacion'.\n";
    cout << "(Metodo campesino ruso de multiplicacion)\n";
    cout << "************************************************************\n";

    string renglon;

    cout << "x: ";
    getline (cin, renglon);
    int x = stoi(renglon);

    bool x_cambio = false;
    if (x < 0) {
        x = -x;
        x_cambio = true;
    }

    cout << "y: ";
    getline (cin, renglon);
    int y = stoi(renglon);

    bool y_cambio = false;
    if (y < 0) {
        y = -y;
        y_cambio = true;
    }

    int prod = 0;

    while (y > 0) {
        if (y % 2 != 0) {
            prod += x;
        }
        x *= 2;
        y /= 2;
    }

    if (x_cambio) {
        prod = -prod;
    }

    if (y_cambio) {
        prod = -prod;
    }

    cout << "x*y=" << prod << endl;

    return 0;
}