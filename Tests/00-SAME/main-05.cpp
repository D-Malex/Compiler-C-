#include <iostream>

using namespace std;

string entero_a_hexa(int n) {
    string hexa = "0";

    if (n != 0) {
        hexa = "";
    }

    string digitos = "0123456789ABCDEF";

    int cociente = n;

    while (cociente != 0) {
        int resto = cociente % 16;

        hexa = digitos.substr(resto, 1) + hexa;

        cociente /= 16;
    }

    return hexa;
}

int main() {
    cout << "*********************************************************************************************\n";
    cout << "Se muestran 16 numeros en decimal y en hexadecimal y, al presionar Enter, se muestran 16 mas.\n";
    cout << "Se sale del programa escribiendo 'salir' y presionando Enter.\n";
    cout << "*********************************************************************************************\n";

    string opcion = "";

    int num = 0;

    while (opcion != "salir" && opcion != "SALIR") {
        cout << "\nDEC\tHEX\n";
        cout << "---\t---\n";

        int cont = 0;

        while (cont < 16) {
            cout << num << "\t" << entero_a_hexa(num) << "\n";
            num += 1;
            cont += 1;
        }

        cout << "Presione Enter o escriba 'salir' y presione Enter: ";
        getline (cin, opcion);

    }

    return 0;
}
