#include <iostream>
#include <cmath>

using namespace std;

const int TRES = 3;

bool es_impar_primo(int n) {
    bool es_p = true;
    int limite = sqrt(n);
    int d = TRES;
    while (d <= limite && es_p) {
        if (n % d == 0) {
            es_p = false;
        }
        d += 2;
    }

    return es_p;
}

int main() {
    cout << "*********************************************************************************************\n";
    cout << "Se ingresa un valor entero positivo, se muestran los numeros primos menores que ese valor.\n";
    cout << "Se utiliza una funcion booleana para determinar si un numero impar mayor que 1 es primo o no.\n";
    cout << "*********************************************************************************************\n";

    string renglon;

    cout << "x: ";
    getline (cin, renglon);
    int x = stoi(renglon);

    if (x <= 2) {
        cout << "No hay numeros primos menores que " << x;
    } else {
        cout << "Numeros primos menores que " << x << ": 2";

        int n = TRES;

        while (n < x) {
            if (es_impar_primo(n)) {
                cout << " " << n;
            }
            n += 2;
        }
    }

    cout << "\n";

    return 0;
}

