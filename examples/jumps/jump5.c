/* Break out of while loop */
int main() {
    int i = 0;
    while (i <= 10) {
        if (i == 6) {
            break;
        } 
        i++;
    }
    return i;
}
