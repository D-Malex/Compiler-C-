#include <iostream>

using namespace std;

void dividir(int x, int y, int& q, int& r) {
    if (y == 0) {
        cout << "ERROR: Division por cero!\n";
    } else {

        q = 0;
        r = x;
        if (r < 0) {
            r = -r;
        }

        int v;
        int w;
        if (y >= 0) {
            v = y;
            w = y;
        } else {
            v = -y;
            w = -y;
        }

        while (w <= r) {
            w *= 2;
        }

        while (w > v) {
            q *= 2;
            w /= 2;
            if (w <= r) {
                r -= w;
                q += 1;
            }
        }

        if (x < 0) {
            r = -r;
            q = -q;
        }

        if (y < 0) {
            q = -q;
        }
    }
}

void mostrar_salida(int cociente, int resto) {
    cout << "Cociente: " << cociente << endl;
    cout << "Resto: " << resto << endl;
}

int main() {
    cout << "**************************************************************\n";
    cout << "Se ingresan dos valores enteros, se muestra su cociente.\n";
    cout << "Se utiliza el algoritmo 'desplazar y restar' (shift-subtract).\n";
    cout << "**************************************************************\n";

    string renglon;

    cout << "x: ";
    getline (cin, renglon);
    int x = stoi(renglon);

    cout << "y: ";
    getline (cin, renglon);
    int y = stoi(renglon);

    int q = 0;
    int r = 0;

    dividir(x, y, q, r);

    if (q != 0 || r != 0) mostrar_salida(q, r);
    
    return 0;
}