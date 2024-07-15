clear all;
clc

% the function on the right-hand side of the equation

f=@(t) (4*pi^2)*sin(2*pi*t);

% number of points in (0,1)

n=99;
h=1/(n+1); % mesh diameter


A=(1/h)*gallery('tridiag',n);   % This is a pre-coded matlab matrix that gives you the right one

% we now declare the right-hand side,and the coordinates in the mesh,
% as we fill them entry by entry
 
rhs=sparse(n,1);
x=zeros(n,1);

for i=1:n
    x(i)=i*h;
end 


for i=1:n
    rhs(i,1)=h*f(x(i));
end

%% Now we solve the syste,=m

solution=A\rhs;

solution = [0;solution;0]; % Add the zero boundary condition

%%
% we solved the problem, now it is time to show the results

% first, we start with building the exact solution (for comparison
% purposes)

xx=linspace(0,1,1000);
u=sparse(1000,1);

uexact=@(t) sin(2*pi*t); 

for i=1:1000
    u(i,1)=uexact(xx(i));
end

figure(1)
x=[0;x;1]; % Add the points 0 and 1 to the x variable
plot(x,solution,'rx-') 
hold on
plot(xx,u,'b-')
