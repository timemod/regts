#include "intpol_cspline.h"
#ifdef DEBUG
#include <stdio.h>
#endif

static void csplin(int n, double x[], double y[], 
                  const char method, double *work[4]);
static void csrand(int n, double x[], double y[], 
                   const char method, double b[4]);
static int csybar(int n, int nbar, double x[], double *work[4],
                  double xbar[], double ybar[]);

int intpol_cspline(int n, int nnew, double x[], double y[],
                   double xnew[], double ynew[], const char method,
                   double *work[4]) {

     // als ierr <> 0, dan is er een fout opgetreden:
     //        ierr = -2: er zijn te weinig datapunten
        
    if (n == 0) {
        return -2;
    } else if (n == 1) {
        // a single point: assume that the timeseries is constant
        for (int i = 0; i < nnew; i++) {
            ynew[i] = y[0];
        }
        return 0;
    }

    csplin(n - 1, x, y, method, work);

#ifdef DEBUG
    printf("After csplin, ierr = %d\n", ierr);
    int i, j;
    printf("work array:\n");
    for (i = 0; i < n; i++) {
        for (j = 0; j < 4; j++) {
            printf("%10g", work[j][i]);
        }
        printf("\n");
    }
#endif

    int ierr = csybar(n - 1, nnew, x, work, xnew, ynew);

    return ierr;
}


static void csplin(int n, double x[], double y[],
              const char method, double *c[4]) {

    /*
     bereken afgeleiden volgens cubic spline methode zdd eerste en
     tweede afgeleiden continu is
     voor i=2 t/m n geldt dan:
 
    (x(i+1)-x(i))*f'(i-1) + 2*(x(i+1)-x(i-1))*f'(i) + (x(i)-x(i-1))*f'(i-1

     = 3*(f(x(i-1),x(i))*(x(i+1)-x(i))+f(x(i),x(i+1))*(x(i)-x(i-1)))

    randvoorwaarden :
         f'(x(1))  + b(1)*f'(x(2)) = b(2)
         f'(x(n+1))+ b(3)*f'(x(n)) = b(4)
         in csrand worden de b-coefficienten bepaald uit bp en type
         n is het aantal te berekenen polynomen.
         er zijn n-1 continue aansluitingen en twee randvoorwaarden.
         er zijn dus in totaal n+1 punten in x
    
    */

    double b[4], f, dxr, dd1, dd3;
    int i, ii;

    for (i = 0; i < n + 1; i++) {
        c[0][i] = y[i];
    }

    // t.b.v. piecewise cubic spline
 
    csrand(n, x, y, method, b);
#ifdef DEBUG
    printf(" b coefficienten csrand:\n");
    for (i = 0; i < 4; i++) {
        printf("%g\n", b[i]);
    }
    printf("\n");
#endif

    // zet voorlopig rechterlid in c2, diagonaal in c3 en subdiag.
    // in c4
    c[2][0] = 1;
    c[3][0] = b[0];
    for (i = 0; i < n; i++) {
        c[3][i + 1] = x[i + 1] - x[i];
        c[2][i + 1] = (c[0][i + 1] - c[0][i]) / c[3][i + 1];
    }
    
    c[1][0] = b[1];
    for (i = 1; i < n; i++) {
        c[1][i] = 3 * (c[3][i] * c[2][i + 1] + c[3][i + 1] * c[2][i]);
        c[2][i] = 2 * (c[3][i] + c[3][i + 1]);
    }
    c[1][n] = b[3];

 
    // begin eliminatie van tridiagonaal en neem direct het rechterlid
    // mee zodat de l niet bewaard behoeft te worden

    for (i = 1; i < n; i++) {
        f = c[3][i + 1] / c[2][i - 1];
        c[2][i] = c[2][i] - f * c[3][i - 1];
        c[1][i] = c[1][i] - f * c[1][i - 1];
    }

    f = b[2] / c[2][n - 1];
    c[1][n] = (c[1][n] -f * c[1][n - 1]) / (1.0 - f * c[3][n - 1]);

    // terugsubstitutie
    for (ii = 2; ii <= n; ii++) {
        i = n + 1 - ii;
        c[1][i] = (c[1][i] - c[3][i] * c[1][i + 1])/ c[2][i];
    }
    c[1][0] = c[1][0] - c[3][0] * c[1][1];

    // bereken coefficienten voor piecewise cubic interpolation
    // als de functie waarden en de afgeleiden gegeven zyn

    for (i = 0; i < n; i++) {
        dxr = 1.0 / (x[i + 1] - x[i]);
        // dd1=first divided difference = f(x(i),x(i+1)) = df/dx
        dd1 = (c[0][i + 1] - c[0][i]) * dxr;
        // dd3=third divided difference = f(x(i),x(i),x(i+1),x(i+1)) *dx*dx
        dd3 = c[1][i] + c[1][i + 1] - 2 * dd1;
        c[2][i] = (dd1 - c[1][i] - dd3) * dxr;
        c[3][i] = dd3 *dxr * dxr;
    }

    return;
}
 
static void csrand(int n, double x[], double y[], 
                   const char method, double b[4]) {
   /* csrand maakt coefficienten b voor randvoorwaarden in csplin
      bij type 'cs..'.
      De twee symbolen achter cs in 'cs..' geven de gewenste
      randvoorwaarden aan het linker resp. rechter uiteinde.
      Hierbij betekent voor het linkeruiteinde :
      blanco    b(1)=.5*bp(1)       b(2)=.5*bp(2)*fdd(1,2)
           0       eerste afgeleide aan de rand = f'(x(1)) = 0
           1       b(1)=bp(1)          b(2)=bp(2)
           2       f'(x(1)) wordt bepaald m.b.v.een parabool door x(1),
                                                           x(2) en x(3)
           3       b(1) en b(2) zodanig dat f'''(x(2)) = continue
           n       natuurlyke spline dwz  b(1)=.5 en b(2)=1.5*fdd(1,2)
           p       parabool aan de rand :  b(1)=1. en b(2)=2. *fdd(1,2)
 
      Voor het rechteruiteinde gelden overeenkomstige regels t.a.v.
      het tweede symbool.
      Als na afloop ierr = -3 dan is het type niet correct.
                   inline functions :
      fdd = first divided difference
     */

    double xx1213;
    int  i, i1, i2, i3;
  
    #define fdd(i, j) ((y[i] - y[j]) / (x[i] - x[j]))
    #define xdx(i, j, k, l) ((x[i] - x[j])/ (x[k] - x[l]))
    
     
    /* bepaal de coefficienten b ; het linkeruiteinde voor
       i=1 en het rechter voor i=3.
       het type randvoorwaarde bevindt zich in rv en de betreffende
       randpunten zijn i1,i2 en i3.
        */
     
    i1 = 0;
    i2 = 1;
    i3 = 2;
    
    for (i = 0; i < 3; i+= 2) {
    
        //  bepaal b(i) en b(i+1)
        //
        switch (method) {

            case '3': b[i] = xdx(i1, i3, i2, i3);
                 xx1213 = xdx(i1, i2, i1, i3);
                 b[i + 1] = fdd(i1, i2) * (2 + xx1213) + fdd(i2, i3) * xx1213 * 
                            xdx(i1, i2, i2, i3);
                 break;

            case 'n': b[i] = 0.5;
                 b[i + 1] = 1.5 * fdd(i1, i2);
                 break;

            default:
                 continue;
                 // TODO: error
        }

    
        if (i == 0) {
            // instellen op rechter-uiteinde
            i1 = n;
            i2 = n - 1;
            i3 = n - 2;
        }
    }
}



static int csybar(int n, int nbar, double x[], double **c,
                  double xbar[], double ybar[]) {

    /*     Bereken nieuwe y-waarden die behoren by nieuwe x-waarden als de
         coefficienten van de partiele derde-graads polynomen bekend zyn.
     
         ybar=c(i,1)+c(i,2)*d+c(i,3)*d**2+c(i,4)*d**3
           d = xbar-x(i)
         x(i) < xbar < x(i+1)
    
        n=aantal intervallen in x waarvoor coeff.in !
        nbar= aantal punten in xbar (input) en ybar (output)
        In xbar behoeven de waarden niet in opklimmende volgorde te staan
        maar de routine maakt gebruik van het feit dat dit meestal zo is.
   */
     
    int in, n1, i, l;
    double dx;
    
    in = 0;
    n1 = n + 1;
    
    for (l = 0; l < nbar; l++) {
    
        // zoek in steeds hogere intervallen
        for (i = in; i < n1; i++) {
            dx = xbar[l] - x[i];
            if (dx < 0) {
                goto l30;
            } else if (dx == 0) {
                goto l100;
            }
        }
        goto l300;

    l30:    if (i == in) goto l50;
            i = i - 1;
            dx = xbar[l] - x[i];
            goto l100;
         
           //  zoek in steeds lagere intervallen
    l50:    if (in == 0) goto l300;
            for (i = in - 1; i >= 0; i--){
                dx = xbar[l] - x[i];
                if (dx >= 0) goto l100;
            }
            goto l300;
        
            // bereken ybar in het i-e interval
    l100:   ybar[l] = c[0][i];
            if (dx > 0) ybar[l] = ybar[l] + dx *(c[1][i] + dx * 
                             (c[2][i] + dx * c[3][i]));
            in = i;

    }
    return 0;
    
    // foutuitgang : xbar(l) ligt niet tussen x(1) t/m x(n1)
l300:   return l;
        
}

