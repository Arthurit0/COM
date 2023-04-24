#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>

#define TSTR 200

char str[TSTR], lookahead;
int pos = 0;

void nextToken()
{
    lookahead = str[pos];
    pos++;
}

void erro(char *msg)
{
   int i;

   printf("%s\n", msg); 
   puts(str);
   for (i = 0; i < pos; i++)
      putchar(' ');
   printf("^\n");
   exit(1);
}

void espacos()
{
    while (lookahead == ' ')
        nextToken();
}

int cons()
{
    char num[10];
    int p = 0;
    
    if (isdigit(lookahead))
    {
        while (isdigit(lookahead))
            if (p < 9)
            {
               num[p++] = lookahead;
               nextToken();
            }
            else
                erro("Constante fora do limite");
        num[p] = '\0';
        return atoi(num);
    }
    else
        erro("Erro sintático");
}
int expr ()
{
    espacos();
    switch (lookahead)
    {
       case '+':
         nextToken(); 
         return (expr() + expr());    
       case '-':
         nextToken(); 
         return (expr() - expr()); 
       case '*':
         nextToken(); 
         return (expr() * expr()); 
       case '/':
         nextToken(); 
         return (expr() / expr()); 
       default:
         return cons();
    }
         
}

int partida()
{
    int e;
    
    e = expr();
    if (lookahead == '\n')
        return e;
    erro ("Erro sintatico");
}

int main()
{
   printf("Expressao:");
   fgets(str, TSTR, stdin);
   nextToken();
   printf("\nResultado = %d\n",partida());
   return 0;
}