// Test switch's basic functionalities

int switch_nobreak() {
    int i = 4;
    int b = 0;

    switch (i) {
        b += 10; // Should be ignored
        case 1:
            b += 1;
        case 2:
            b += 2;
        case 3:
            b += 3;
        case 4:
            b += 4;
        case 5:
            b += 5;

        b += 6;
    }

    return b; // Expect: 15
}

int switch_break() {
    int i = 4;
    int b = 0;

    switch (i) {
        b += 10; // Should be ignored
        case 1:
            b += 1;
        case 2:
            b += 2;
        case 3:
            b += 3;
        case 4:
            b += 4;
            break;
        case 5:
            b += 5;

        b += 6;
    }

    return b; // Expect: 4
}

int switch_default() {
    int i = 7;
    int b = 0;

    switch (i) {
        b += 10; // Should be ignored
        case 4:
            b += 4;
            break;
        case 5:
            b += 5;
            break;
        default:
            b += 6;
    }

    return b; // Expect: 6
}

int switch_default_fallback() {
    int i = 5;
    int b = 0;

    switch (i) {
        b += 10; // Should be ignored
        case 4:
            b += 4;
            break;
        case 5:
            b += 5;
        default:
            b += 6;
    }

    return b; // Expect: 11
}

int main() {
    if (switch_nobreak() != 15 || switch_break() != 4 
        || switch_default() != 6 || switch_default_fallback() != 11) {
        return 0;
    } else {
        return 1;
    }
}
