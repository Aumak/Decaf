extern "C" {
    int read_int();
    void print_int(int x);
    void print_string(const char* str);
}

int main() {
    int i = read_int();                   // input number from user
    print_string("this is a test: ");     // print label
    print_int(i);                         // print the number
    print_string("\n");                   // print newline
    return 0;
}
