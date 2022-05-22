#ifndef COMMON_HPP
#define COMMON_HPP
 

/* DO NOT MODIFY THE CLASS COORDINATE */
class Coordinate {
public:
  Coordinate(double x_other, double y_other);
  Coordinate();
  Coordinate(const Coordinate &other);
  ~Coordinate();

  double get_x() const;
  double get_y() const;
  void set_x(const double x);
  void set_y(const double y);
  double get_distance(Coordinate other);

  Coordinate operator+(const Coordinate& other);
  Coordinate operator-();
  Coordinate operator-(const Coordinate& other);

  bool operator==(const Coordinate& other);
  bool operator!=(const Coordinate& other);
  bool operator>(const Coordinate& other);
  bool operator<(const Coordinate& other);

  static int get_num_instances();

  friend std::ostream& operator<<(std::ostream& os, const Coordinate& c);

private:
  double x;
  double y;
  static int num_instances;
};


class Projectile {
public:
  Projectile(Coordinate position_other, double magnitude, double angle);
  Projectile();
  virtual ~Projectile();

  Coordinate get_position() const;
  double get_velocity_x();
  double get_velocity_y();
  void simulate_step(double simulation_interval);
  bool operator==(const Projectile& other);
  bool operator!=(const Projectile& other);

  friend std::ostream& operator<<(std::ostream& os, const Projectile& c);
private:
  Coordinate position;
  double velocity_x;
  double velocity_y;
  double init_magnitude;
  double init_angle;
};

class ProjectileListNode {
public:
  ProjectileListNode(Projectile projectile);

  Projectile get_projectile();
  void set_next(ProjectileListNode* next);
  void set_prev(ProjectileListNode* prev);
  ProjectileListNode* get_next();
  ProjectileListNode* get_prev();

private:
  Projectile element;
  ProjectileListNode *next, *prev;
};

class ProjectileList {
public:
  ProjectileList();
  ~ProjectileList();

  bool is_empty();
  void append(Projectile projectile);
  Projectile remove_from_head();
  Projectile remove_from_tail();

private:
  ProjectileListNode *head, *tail;
};


int count_coordinate_instances(Coordinate c1, Coordinate c2, double min_distance);

#endif
