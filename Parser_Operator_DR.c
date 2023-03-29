#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#define TSTR 200

char str[TSTR], numStr[TSTR], lookahead;
int pos = 0, numPos = 0, answer;

void nextToken() {
    lookahead = str[pos];
    pos++;
}

void cleanNum() {
    for (int i = 0; i < TSTR; i++) {
        numStr[i] = 'O';
    }
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
            numStr[numPos++] = lookahead;
            nextToken();
        }
        numPos = 0;
    } else
        erro("Erro sintático");
}
int expr() {
    int num1, num2, resp;
    char opr;

    espacos();
    if (!isdigit(lookahead)) {
        opr = lookahead;
        nextToken();
        num1 = expr();
        cleanNum();
        num2 = expr();
    } else {
        cons();
        return atoi(numStr);
    }

    switch (opr) {
        case '+':
            resp = num1 + num2;
            break;
        case '-':
            resp = num1 - num2;
            break;
        case '*':
            resp = num1 * num2;
            break;
        case '/':
            resp = num1 / num2;
            break;
        default:
            resp = __INT_MAX__;
    }

    if (resp != __INT_MAX__)
        return resp;
    else
        erro("Operacao Invalida");
}

int main() {
    printf("Expr: ");
    fgets(str, TSTR, stdin);
    cleanNum();
    nextToken();
    answer = expr();
    printf("\nResposta = %d\n\n", answer);
    return 0;
}