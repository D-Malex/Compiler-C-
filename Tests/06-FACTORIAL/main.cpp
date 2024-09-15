#include <iostream>

using namespace std;

int factorial(int n) {
    if (n < 2) {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}

int main() {
    cout << "****************************************************\n";
    cout << "Se ingresa un valor entero, se muestra su factorial.\n";
    cout << "Se utiliza un algoritmo recursivo.\n";
    cout << "****************************************************\n";

    string renglon;

    cout << "n: ";
    getline (cin, renglon);
    int n = stoi(renglon);

    if (n < 0) {
        cout << "ERROR: El algoritmo requiere un numero entero no negativo!\n";
    } else {
        cout << n << "! es " << factorial(n) << endl;
    }

    return 0;
}
