xp = [1:5];
yp = [1, 3, 4, 4, 1];

n = 17;
x = [1: 4/(17-1): 5]

res = interp1(xp, yp, x, "spline")

csvwrite("output/interp_example1.csv", res);


