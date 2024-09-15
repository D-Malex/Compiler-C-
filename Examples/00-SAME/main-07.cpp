#include <iostream>

using namespace std;

double potencia_rec(double x, int n) {
    if (n == 0) {
        return 1.0;
    }
    if (n % 2 == 0) {
        double m = potencia_rec(x, n / 2);
        return m * m;
    } else {
        return x * potencia_rec(x, n - 1);
    }
}

double potencia(double x, int n) {
    double p;
    if (n < 0) {
        p = potencia_rec(x, -n);
        return 1.0 / p;
    } else {
        return potencia_rec(x, n);
    }
}

int main() {
    cout << "*******************************************************************************************\n";
    cout << "Se ingresan el valor de una base y el valor entero de un exponente, se muestra la potencia.\n";
    cout << "Se utiliza un algoritmo recursivo.\n";
    cout << "*******************************************************************************************\n";

    string renglon;

    cout << "b: ";
    getline (cin, renglon);
    double b = stof(renglon);

    cout << "e: ";
    getline (cin, renglon);
    int e = stoi(renglon);

    cout << b << " elevado a la " << e << " es " << potencia(b, e) << endl;

    return 0;
}
