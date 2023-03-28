#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#define TSTR 200

char str[TSTR], lookahead, val[TSTR];
int pos = 0, length = 0;

void nextToken() {
    lookahead = str[pos];
    pos++;
}

void erro(char *msg) {
    int i;

    printf("%s\n", msg);
    puts(str);
    for (i = 0; i < pos; i++)
        putchar(' ');
    printf("^\n");
    exit(1);
}

void espacos() {
    while (lookahead == ' ')
        nextToken();
}

void cons() {
    if (isdigit(lookahead)) {
        while (isdigit(lookahead)) {
            val[length++] = lookahead;
            nextToken();
        }
    } else
        erro("Erro sintático\n");
}
char expr() {
    int op1, op2;

    espacos();
    switch (lookahead) {
        case '+':
        case '-':
        case '*':
        case '/':
            nextToken();
            op1 = expr();
            op2 = expr();
            break;
        default:
            cons();
    }
    return (int)val;
}

int partida() {
    char e = expr();
    if (lookahead == '\n') {
        return e;
    } else {
    }

    erro("Erro sintático");
}

int main() {
    printf("Expressao: ");

    fgets(str, TSTR, stdin);
    printf("%s", str);
    nextToken();
    partida();
    printf("\nExpressao Correta\n");
    return 0;
}