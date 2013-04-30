%{
#include<stdio.h>
#include<string.h>
int x=0,t=0,in=0;
float fl=0;
int a=0;
int line=1;
struct node
{
        char name[16];
        union value{
                        float f;
                        int i;
                        char c;
                }val;
        int dtype;
        struct node *link;
}*first=NULL,*tmp, *crt;
struct node * checksym(char *);
void addsymbol(struct node *,char *);
void addfn0(struct node *, int, int);
void addfn1(struct node *, int, float);
void addfn2(struct node *, int, char);
void checkvalid(struct node *);
void printsymtable();
struct node* fv(struct node *);
%}

%token VOID MAIN CE
%token FOR IF ELSE WHILE
%left '+' '-'
%left '/' '*'

%union
{
        int ival;
        float fval;
        char cval;
        struct node *ptr;
}

%token <ival> INTC
%token <fval> FLTC
%token <cval> CHRC
%token <ival> CHAR
%token <ival> INT
%token <ival> FLOAT
%token <ptr> VAR
%type <ptr> E
%type <ptr> T
%type <ptr> F
%type <ival> lp1 lp2

%%

program: VOID MAIN '(' ')' blk{
                printsymtable();
                return;
                }
        ;

blk:    '{' sts '}'
        | '{' '}'
        ;

sts:    sts st
        | st
        ;

st:     ifst
        | whst
        | frst
        | decl
        | blk
        | exp
        | ';'
        ;

whst:   WHILE '(' fl ')' st{ printf("While\n"); }
        ;

frst:   FOR '(' fl ';' fl ';' fl ')' st{ printf("For\n");       }
        ;

ifst:   IF '(' fl ')' st{ printf("If\n");       }
        | IF '(' fl ')' st ELSE st{ printf("If-else\n");        }
        ;

fl:     comp
        | exp
        ;

decl:   INT VAR lp1{
                if($2->dtype!=-1)
                        printf("Multiple Declaration\n");
                else
                {
                        addfn0($2,$1,0); x=$1;
                //      printf("INT Declaration found\n");
                }
                }
        | INT VAR '=' INTC lp1{
                if($2->dtype!=-1)
                        printf("Multiple Declaration\n");
                else
                {
                        addfn0($2,$1,$4); x=$1;
                //      printf("INT Declaration found\n");
                }
                }
        | FLOAT VAR lp2{
                if($2->dtype!=-1)
                        printf("Multiple Declaration\n");
                else
                {
                        addfn1($2,$1,0); x=$1;
                //      printf("FLOAT Declaration found\n");
                }
                }
        | FLOAT VAR '=' FLTC lp2{
                if($2->dtype!=-1)
                        printf("Multiple Declaration\n");
                else
                {
                        addfn1($2,$1,$4); x=$1;
                //      printf("FLOAT Declaration found\n");
                }
                }
        | CHAR VAR lp3{
                if($2->dtype!=-1)
                        printf("Multiple Declaration\n");
                else
                {
                        addfn2($2,$1,0); x=$1;
                //      printf("CHAR Declaration found\n");
                }
                }
        | CHAR VAR '=' CHRC lp3{
                if($2->dtype!=-1)
                        printf("Multiple Declaration\n");
                else
                {
                        addfn2($2,$1,$4); x=$1;
                //      printf("CHAR Declaration found\n");
                }
                }
        ;

lp1:    ',' VAR lp1{
                if($2->dtype!=-1)
                        printf("Multiple Declaration\n");
                else
                {
                        addfn0($2,0,0);
                //      printf("INT Declaration found\n");
                }
                }
        | ',' VAR '=' INTC lp1{
                if($2->dtype!=-1)
                        printf("Multiple Declaration\n");
                else
                {
                        addfn0($2,0,$4);
                //      printf("INT Declaration found\n");
                }
                }
        | ';'
        ;

lp2:    ',' VAR lp2{
                if($2->dtype!=-1)
                        printf("Multiple Declaration\n");
                else
                {
                        addfn1($2,1,0.0);
                //      printf("FLOAT Declaration found\n");
                }
                }
        | ',' VAR '=' FLTC lp2{
                if($2->dtype!=-1)
                        printf("Multiple Declaration\n");
                else
                {
                        addfn1($2,1,$4);
                //      printf("FLOAT Declaration found\n");
                }
                }
        | ';'
        ;

lp3:    ',' VAR lp3{
                if($2->dtype!=-1)
                        printf("Multiple Declaration\n");
                else
                {
                        addfn2($2,x,0);
                //      printf("CHAR Declaration found\n");
                }
                }
        | ',' VAR '=' CHRC lp3{
                if($2->dtype!=-1)
                        printf("Multiple Declaration\n");
                else
                {
                        addfn2($2,x,$4);
                //      printf("CHAR Declaration found\n");
                }
                }
        | ';'
        ;

comp:   E CE E
        ;

exp:    VAR '=' E ';'{
                if($1->dtype==0)
                {
                        if($3->dtype==0)
                                $1->val.i=$3->val.i;
                        else
                                $1->val.i=(int)$3->val.f;
                }
                else if($1->dtype==1)
                {
                        if($3->dtype==1)
                                $1->val.f=$3->val.f;
                        else
                                $1->val.f=(float)$3->val.i;
                }
                else
                        printf("Variable undeclared\n");
                }
        | VAR '=' CHRC ';'{
                if($1->dtype!=2)
                        printf("Invalid datatype");
                else
                        $1->val.c=$3;
                }
        ;

E:      E '+' T{
                tmp=checksym("tmp");
                if(($1->dtype)!=($3->dtype))
                {
                        tmp->dtype=1;
                        printf("Datatype mismatch in line : %d\nPerforming error correction\n",line);
                        if((tmp->dtype)==1)
                        {
                                tmp->val.f=($1->val.f)+($3->val.i);
                        }
                        else
                        {
                                tmp->val.f=($1->val.i)+($3->val.f);
                        }
                        $$=tmp;
                }
                else
                {
                        if($1->dtype==0)
                        {
                                tmp->dtype=0;
                                tmp->val.i=($1->val.i)+($3->val.i);
                        }
                        else if($1->dtype==1)
                        {
                                tmp->dtype=1;
                                tmp->val.f=(tmp->val.f)+($3->val.f);
                        }
                        else
                                printf("Invalid Datatype\n");
                        $$=tmp;
                }
                }
        | E '-' T{
                tmp=checksym("tmp");
                if(($1->dtype)!=($3->dtype))
                {
                        tmp->dtype=1;
                        printf("Datatype mismatch in line : %d\nPerforming error correction\n",line);
                        if((tmp->dtype)==1)
                        {
                                tmp->val.f=($1->val.f)-($3->val.i);
                        }
                        else
                        {
                                tmp->val.f=($1->val.i)-($3->val.f);
                        }
                        $$=tmp;
                }
  	else
                {
                        if($1->dtype==0)
                        {
                                tmp->dtype=0;
                                tmp->val.i=($1->val.i)-($3->val.i);
                        }
                        else if($1->dtype==1)
                        {
                                tmp->dtype=1;
                                tmp->val.f=(tmp->val.f)-($3->val.f);
                        }
                        else
                                printf("Invalid Datatype\n");
                        $$=tmp;
                }
                }

        | T{ $$=$1;     }
        ;

T:      T '*' F{
                tmp=checksym("tmp");
                if(($1->dtype)!=($3->dtype))
                {
                        tmp->dtype=1;
                        printf("Datatype mismatch in line : %d\nPerforming error correction\n",line);
                        if((tmp->dtype)==1)
                        {
                                tmp->val.f=($1->val.f)*($3->val.i);
                        }
                        else
                        {
                                tmp->val.f=($1->val.i)*($3->val.f);
                        }
                        $$=tmp;
                }
                else
                {
                        if($1->dtype==0)
                        {
                                tmp->dtype=0;
                                tmp->val.i=($1->val.i)*($3->val.i);
                        }
                        else if($1->dtype==1)
                        {
                                tmp->dtype=1;
                                tmp->val.f=(tmp->val.f)*($3->val.f);
                        }
                        else
                                printf("Invalid Datatype\n");
                        $$=tmp;
                }
                }
        | T '/' F{
                tmp=checksym("tmp");
                if(($1->dtype)!=($3->dtype))
                {
                        tmp->dtype=1;
                        printf("Datatype mismatch in line : %d\nPerforming error correction\n",line);
                        if((tmp->dtype)==1)
                        {
                                tmp->val.f=($1->val.f)/($3->val.i);
                        }
                        else
                        {
                                tmp->val.f=($1->val.i)/($3->val.f);
                        }
                        $$=tmp;
                }
                else
                {
                        if($1->dtype==0)
                        {
                                tmp->dtype=0;
                                tmp->val.i=($1->val.i)/($3->val.i);
                        }
                        else if($1->dtype==1)
                        {
                                tmp->dtype=1;
                                tmp->val.f=(tmp->val.f)/($3->val.f);
                        }
                        else
                                printf("Invalid Datatype\n");
                        $$=tmp;
                }
                }
        | F{ $$=$1;     }
        ;

F:      '(' VAR ')'{
                if($2->dtype==-1)
                        printf("Variable %s undeclared\n",$2->name);
                else
                        $$=$2;
                }
        | VAR{  printf("4\n");
                if($1->dtype==-1)
                        printf("Variable %s undeclared\n",$1->name);
                else
                        $$=$1;
                }
        | '(' INTC ')'{
                $$=checksym("tmp");
                $$->val.i=$2;
                $$->dtype=0;
                }
        | INTC{
                $$=checksym("tmp");
                $$->val.i=$1;
                $$->dtype=0;
                }
        | '(' FLTC ')'{
                $$=checksym("tmp");
                $$->val.f=$2;
                $$->dtype=1;
                }
        | FLTC{
                $$=checksym("tmp");
                $$->val.f=$1;
                $$->dtype=1;
                }
        ;

%%

void addsymbol(struct node *tp,char *vname)
{
        strcpy(tp->name,vname);
        tp->dtype=-1;
        tp->link=NULL;
}

struct node * checksym(char *vname)
{
        struct node *ftp;
        struct node *rp;
        struct node *nnode;
        if(first==NULL)
        {
                nnode=(struct node *)malloc(sizeof(struct node));
                addsymbol(nnode,vname);
                first=nnode;
        }
        else
        {
                ftp=first;
                while(ftp!=NULL)
                {
                        if(strcmp(vname,ftp->name)==0)
                        {
                                return ftp;
                        }
                        rp=ftp;
                        ftp=ftp->link;
                }
                nnode=(struct node *)malloc(sizeof(struct node));
                addsymbol(nnode,vname);
                rp->link=nnode;
        }
        return nnode;
}

void addfn1(struct node *t,int type,float val)
{
        if(t->dtype==-1)
        {
                t->dtype=type;
                t->val.f=(float)val;
        }
        else
                printf("Redeclaration of variable %s\n",t->name);
}

void addfn0(struct node *t,int type,int val)
{
        if(t->dtype==-1)
        {
                t->dtype=type;
                t->val.i=val;
        }
        else
                printf("Redeclaration of variable %s\n",t->name);
}

void addfn2(struct node *t,int type,char val)
{
        if(t->dtype==-1)
        {
                t->dtype=type;
                t->val.c=val;
        }
        else
                printf("Redeclaration of variable %s\n",t->name);
}

struct node* fv(struct node *t)
{
        if(t->dtype==-1)
                return NULL;
        else
                return t;
}

void checkvalid(struct node *t)
{
        if(t->dtype==-1)
        {
                printf("Variable \"%s\" undeclared\n",t->name);
        }
}

void printsymtable()
{
        struct node *ftp;
        ftp=first;
        printf("\n\nSymbol Table:\n");
        while(ftp!=NULL)
        {
                printf("name=%s\tdatatype=%d\t",ftp->name,ftp->dtype);
                if(ftp->dtype==0)
                        printf("value=%d\n",ftp->val.i);
                if(ftp->dtype==1)
                        printf("value=%f\n",ftp->val.f);
                if(ftp->dtype==2)
                        printf("value=%c\n",ftp->val.c);
                ftp=ftp->link;
        }
}

int yyerror(char *s)
{
        printf("Line - %d : %s\n",line,s);
        return 0;
}

int main()
{
        yyparse();
        return 0;
}
