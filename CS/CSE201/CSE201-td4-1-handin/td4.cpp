#include <iostream>
#include <math.h>
#include <stdlib.h>     // include rand

#include "td4.hpp"

double PI = 3.14159265;
const double G_CONSTANT = 9.8;

Coordinate::Coordinate()
{
    x = 0;
    y = 0;
}

Coordinate::Coordinate(double x1, double y1)
{
    x = x1;
    y = y1;
}

double Coordinate::get_x()
{
    return x;
}

double Coordinate::get_y()
{
    return y;
}

double Coordinate::get_distance(Coordinate a)
{
    return sqrt((x - a.x) * (x - a.x) + (y - a.y) * (y - a.y));
}

Target::Target()
{
    x = 0;
    y = 0;
    r = 1;
}

Target::Target(Coordinate c, double r1)
{
    x = c.get_x();
    y = c.get_y();
    r = r1;
}

Coordinate Target::get_position()
{
    return Coordinate(x ,y);
}

double Target::get_radius()
{
    return r;
}

void Target::randomize()
{
    x = rand() % 100;
    y = rand() % 100;
}

Projectile::Projectile()
{
    x = 0, y = 0;
    vx = sqrt(2) / 2;
    vy = sqrt(2) / 2;
}

Projectile::Projectile(Coordinate p, double m, double a)
{
    x = p.get_x();
    y = p.get_y();
    vx = m * cos(a * PI / 180);
    vy = m * sin(a * PI / 180);
}

Coordinate Projectile::get_position()
{
    return Coordinate(x, y);
}

double Projectile::get_velocity_x()
{
    return vx;
}

double Projectile::get_velocity_y()
{
    return vy;
}

void Projectile::simulate_step(double t)
{
    vy = vy - G_CONSTANT * t;
    x = x + vx * t;
    y = y + vy * t - 0.5 * G_CONSTANT * t * t;
}

bool Projectile::intersect(Target p)
{
    Coordinate pos = p.get_position();
    double dis = pos.get_distance(Coordinate(x, y));
    return dis <= p.get_radius();
}

Telemetry::Telemetry()
{
    x = new int[1000];
    y = new int[1000];
    t = new int[1000];
    tot = 0;
}

Telemetry::~Telemetry()
{
    delete [] x;
    delete [] y;
    delete [] t;
}

int Telemetry::get_tot_points()
{
    return tot;
}

void Telemetry::add_point(double T, double X, double Y)
{
    x[tot] = X;
    y[tot] = Y;
    t[tot++] = T;
}

void Telemetry::get_point(int i, double &T, double &X, double &Y)
{
    X = x[i];
    Y = y[i];
    T = t[i];
}


void Game::run(double simulation_interval)
{
    Coordinate pos = projectile.get_position();
    while (projectile.intersect(target) != true && pos.get_y() >= 0)
    {
        telemetry.add_point(time, pos.get_x(), pos.get_y());
        projectile.simulate_step(simulation_interval);
        time += simulation_interval;
        pos = projectile.get_position();
    }
}
