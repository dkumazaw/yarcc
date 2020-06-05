/* Multiple breaks in loop should be ok */
int main() {
    int i;
    for (i =0; i < 20; i++) {
        if (i & 8) break;
        if (i & 4) break;
    }
    return i;
}
