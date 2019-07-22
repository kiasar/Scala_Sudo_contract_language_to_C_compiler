# Scala Sudo contract language to c++ compiler
this is a Scala code that compiles a Sudo contract language to C++, you can use it for learning Scala and see some sample code.

## What is this Sudo language?
This language is for defining contracts, let me give you some examples and after that show you all functions.
###### Date: this is how you define a Date of "day 100" and "hour 16":
    t1 :: Date
    t1 = mkdate(100 ,  15)
###### the difference of date: this is how you find the difference of two dates (the time from this date to that date):
    # t1 and t2 are dates
    d1 :: Int
    d1 = diff( t1 ,  t2 )
###### Contracts: Now! this is a contract that means "you must pay 100$ before the date t1":
    c1  ::  Contract
    c1 = zcb ( t1 ,  100)
###### Scale: you can scale the amount of the money of a contract with this:
    # c1 is a contract
    scale (20 ,  c1)
###### one: this is a contract with value 1$ and time = inf:
    c1 :: Contract
    c1 = one()
###### give: will reverlce the costumer and the dealer of an contract:
    # c1 is a contract
    c2 :: Contract
    c2 = give(c1)
##### and other things like "and", "or", "then", "TimeFunc" and so on that is in this language.
now you that you have a general idea about it I will show you all the functions. If you can read Persian so you can read the full descriptions of this Sudo Cotrant language in [here](https://d1b10bmlvqabco.cloudfront.net/attach/j7r7avrmonu3ul/hm63qs7fzfh1ep/jcly3wx21t9x/project_v5.pdf)

### The Grammar of the language:

> <NAME>::=[[a-z]]+[[a-z | 0-9]]*
<TYPE>::=[[Int | Double | Date | Contract]]
<DEFFUNC>::=  ([[<TYPE>|<TYPE>[[,<TYPE>]]*]]) -><TYPE>
<DEFVAR>::=<TYPE><DEF>::=<NAME>::[[<DEFVAR>|<DEFFUNC>|<DEFTIMEFUNC>]]
<DEFTIMEFUNC>::=  TimeFunc (Date) -> Double
<ASSIGN>::=<NAME>=<EXPR>
<INT>::=[[<NAME>|[[0-9]]+]]
<DOUBLE>::=[[<NAME>|[[0-9]]+.[[0-9]]*]]
<DATE>::=<NAME>
<EXPR>::=[[<INT>|<DATE>|<DOUBLE>]]
<EXPR>::=<EXPR>[[* | + | - | /]]<EXPR>
<EXPR>::=  (<EXPR>)
<EXPR>::=<FUNCCALL>
<ARGS>::=[[<ARG>|<ARG>[[,<ARG>]]*]]
<ARG>::=<EXPR>
<FUNCCALL>::=<NAME>(<ARGS>)
<FUNCCALL>::=  mkdate(<ARG>,<ARG>)
<FUNCCALL>::=[[and | or | then | scaleX | scale | truncate]](<ARG>,<ARG>)
<FUNCCALL>::=  one()
<FUNCCALL>::=  give(<ARG>)
<PROGRAM>::=[[<ASSIGN>|<FUNCCALL>|<DEF>]]*

## what does this code do?
this Scala code will compile this language to a C++ code, so you can write your contract on that Sudo language then compile it to C++ and run it. for example if your code in this Sudo language is this:
```
c1 :: Contract
c1 = one()
c2 :: Contract
c2 = give(c1)
c3 :: Contract
c3 = scale(20, one())
END
3	20
c1
c2
c3
```
this code will give you this:
```c++
#include <iostream>
using namespace std;
#define InfDate 365*24
int today;
int start;
class Contract{
public:
    int exDate;
    double value;
    Contract(int exDate, double value){
        this->exDate = exDate;
        this->value = value;
    }
    Contract(){
    }
    double getVal(){
        if (today > exDate) return 0;
        else return value;
    }
};

int defMakeDate(int day, int hour){
    return (day -1) * 24 + hour;
}

Contract defOne(){
    return Contract(InfDate, 1);
}

Contract defGive(Contract con){
    return Contract(con.exDate, -1 * con.value);
}

Contract defscale(double scale, Contract con){
    return Contract(con.exDate, scale * con.value);
}

Contract deftruncate(int date, Contract con){
    if (date < con.exDate) return Contract(date, con.value);
    else return Contract(con.exDate, con.value);
}

Contract defand(Contract con1, Contract con2){
	if(con1.exDate >= con2.exDate) return Contract(con1.exDate, con1.getVal() + con2.getVal());
    else return Contract(con2.exDate, con1.getVal() + con2.getVal());
}

Contract defthen(Contract con1, Contract con2){
	if(con1.exDate >= con2.exDate) return Contract(con1.exDate, con1.getVal());
	else if(start > con1.exDate) return Contract(con2.exDate, con2.getVal());
	else if(con1.getVal() > con2.getVal()) return Contract(con2.exDate, con1.getVal());
	else Contract(con2.exDate, con2.getVal());
}

Contract defscaleX(double d, Contract con){
     return Contract(con.exDate, con.value * d);
}

```


