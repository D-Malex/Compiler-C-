#include <iostream>

using namespace std;

int mcd(int x, int y) {
    while (x != y) {
        if (x < y) {
            y -= x;
        }
        if (y < x) {
            x -= y;
        }
    }

    return x;
}

int main() {
    cout << "******************************************************************************\n";
    cout << "Se ingresan dos valores enteros positivos, se muestra su maximo comun divisor.\n";
    cout << "Se utiliza el algoritmo de Euclides.\n";
    cout << "******************************************************************************\n";

    string renglon;

    cout << "x: ";
    getline (cin, renglon);
    int x = stoi(renglon);

    cout << "y: ";
    getline (cin, renglon);
    int y = stoi(renglon);

    if (x <= 0 || y <= 0) {
        cout << "ERROR: El algoritmo requiere dos numeros enteros positivos!\n";
        return 1;
    }

    cout << mcd(x, y) << " es el MCD entre ";

    cout << x << " y " << y << endl;

    return 0;
}
