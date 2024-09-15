#include <iostream>
#include <cmath>

using namespace std;

void mostrar_espacios(int n) {
    int i = 0;

    while (i < n) {
        cout << " ";
        i += 1;
    }
}

int main() {
    cout << "*******************************************************\n";
    cout << "Se muestra una sinusoide graficada mediante asteriscos.\n";
    cout << "*******************************************************\n";

    double a = 0.0;

    double lim = 8.0 * atan(1.0);

    while (a < lim) {
        double s = sin(a);
        mostrar_espacios(25.0 + 24.0 * s);
        cout << "* sin(" << a << ") = " << s << endl;
        a += 0.1;
    }

    mostrar_espacios(25);
    cout << "* sin(" << lim << ") = " << 0.0 << endl;
 
    return 0;
}
