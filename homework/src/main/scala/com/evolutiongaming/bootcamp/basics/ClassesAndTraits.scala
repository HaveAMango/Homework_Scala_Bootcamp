package com.evolutiongaming.bootcamp.basics

object ClassesAndTraits {


  class MutablePoint(var x: Double, var y: Double) {
    def move(dx: Double, dy: Double): Unit = {
      x = x + dx
      y = y + dy
    }

    override def toString: String =
      s"($x, $y)"
  }

  val point1 = new MutablePoint(3, 4)
  println(point1.x) // 3.0
  println(point1)   // (3.0, 4.0)

  sealed trait Shape extends Located with Bounded with Movable {

    def area: Double
  }

  sealed trait Located { // protected interface Located {
    def x: Double        //     Double x();
    def y: Double        //     Double y();
  }                      // }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Movable {
    def move(dx: Double, dy: Double): Shape
  }


  final case class Point(x: Double, y: Double) extends Shape {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def move(dx: Double, dy: Double): Point = Point(x + dx, y + dy)

    override def area: Double = 0
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def move(dx: Double, dy: Double): Circle = Circle(centerX + dx, centerY + dy, radius)

    override def area: Double = Math.PI * radius * radius
  }

  final case class Square (override val centerX: Double, override val centerY: Double, side: Double)
    extends Rectangle(centerX = centerX, centerY = centerY, width = side, height = side) {
    override def move(dx: Double, dy: Double): Square = Square(centerX + dx, centerY + dy, side)
  }

  case class Rectangle(centerX: Double, centerY: Double, width: Double, height: Double) extends Shape{
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = x - width / 2
    override def maxX: Double = x + width / 2
    override def minY: Double = y - height / 2
    override def maxY: Double = y + height / 2
    override def move(dx: Double, dy: Double): Rectangle = Rectangle(centerX + dx, centerY + dy, width, height)

    override def area: Double = width * height
  }

  final case class Triangle (a: Point, b: Point, c: Point) extends Shape{
    override def x: Double = (a.x + b.x + c.x) / 3
    override def y: Double = (a.y + b.y + c.y) / 3
    override def minX: Double = Math.min(a.x, Math.min(b.x, c.x))
    override def maxX: Double = Math.max(a.x, Math.max(b.x, c.x))
    override def minY: Double = Math.min(a.y, Math.min(b.y, c.y))
    override def maxY: Double = Math.max(a.y, Math.max(b.y, c.y))
    override def move(dx: Double, dy: Double): Triangle = Triangle(a.move(dx, dy), b.move(dx, dy), c.move(dx, dy))
    override def area: Double = ???
  }

  sealed trait Shape3D extends Located3d with Bounded3d with Movable3d {
    def surfaceArea: Double

    def volume: Double
  }
  sealed trait Located3d {
    def x: Double
    def y: Double
    def z: Double
  }

  sealed trait Bounded3d {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
    def maxZ: Double
    def minZ: Double
  }

  sealed trait Movable3d {
    def move(dx: Double, dy: Double, dz: Double): Shape3D
  }

  final case class Point3d (x: Double, y: Double, z: Double) extends Shape3D {
    override def surfaceArea: Double = 0
    override def volume: Double = 0
    override def move(dx: Double, dy: Double, dz: Double): Point3d = Point3d(x+ dx, y+dy, z+dz)
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def maxZ: Double = z
    override def minZ: Double = z
  }

  case class Cuboid(centerX: Double, centerY: Double, centerZ: Double, width: Double, height: Double, depth: Double) extends Shape3D{
    override def surfaceArea: Double = 2*(width*depth + depth*height + height*width)

    override def volume: Double = width * height * depth

    override def move(dx: Double, dy: Double, dz: Double): Shape3D = Cuboid(centerX + dx, centerY + dy, centerZ + dz, width, height, depth)

    override def minX: Double = x - width / 2
    override def maxX: Double = x + width / 2
    override def minY: Double = y - height / 2
    override def maxY: Double = y + height / 2

    override def maxZ: Double = z + depth / 2
    override def minZ: Double = z - depth / 2
    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ
  }

  final case class Cube (override val centerX: Double, override val centerY: Double, override val centerZ: Double, side: Double)
    extends Cuboid(centerX = centerX, centerY = centerY, centerZ = centerZ, width = side, height = side, depth = side) {
    override def move(dx: Double, dy: Double, dz: Double): Shape3D = Cube(centerX + dx, centerY + dy, centerZ + dz, side)
  }

  final case class Triangle3D (a: Point3d, b: Point3d, c: Point3d) extends Shape3D {
    override def surfaceArea: Double = ???
    override def volume: Double = ???

    override def x: Double = (a.x + b.x + c.x ) / 3
    override def y: Double = (a.y + b.y + c.y ) / 3
    override def z: Double = (a.z + b.z + c.z ) / 3
    override def minX: Double = Math.min(a.x, Math.min(b.x, c.x))
    override def maxX: Double = Math.max(a.x, Math.max(b.x, c.x))
    override def minY: Double = Math.min(a.y, Math.min(b.y, c.y))
    override def maxY: Double = Math.max(a.y, Math.max(b.y, c.y))
    override def minZ: Double = Math.min(a.z, Math.min(b.z, c.z))
    override def maxZ: Double = Math.max(a.z, Math.max(b.z, c.z))
    override def move(dx: Double, dy: Double, dz: Double): Triangle3D = Triangle3D(a.move(dx, dy, dz), b.move(dx, dy, dz), c.move(dx, dy, dz))

  }

  final case class Sphere(centerX: Double, centerY: Double, centerZ: Double, radius: Double) extends Shape3D {
    override def surfaceArea: Double = ???
    override def volume: Double = ???

    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def maxZ: Double = z + radius
    override def minZ: Double = z - radius

    override def move(dx: Double, dy: Double, dz: Double): Sphere = Sphere(centerX + dx, centerY + dy, centerZ + dz, radius)
  }



  val point2 = Point(1, 2)
  println(point2.x)

  val shape: Shape = point2
  val point2Description = shape match {
    case Point(x, y)  => s"x = $x, y = $y"
    case _            => "other shape"
  }

  val point3 = point2.copy(x = 3)
  println(point3.toString) // Point(3, 2)

  def minimumBoundingRectangle(objects: Set[Bounded]): Bounded = {
    new Bounded {
      implicit private val doubleOrdering: Ordering[Double] = Ordering.Double.IeeeOrdering


      override def minX: Double = objects.map(_.minX).min
      override def maxX: Double = objects.map(_.maxX).min
      override def minY: Double = objects.map(_.minY).min
      override def maxY: Double = objects.map(_.maxY).min
    }
  }

  // Pattern matching and exhaustiveness checking
  def describe(x: Shape): String = x match {
    case Point(x, y) => s"Point(x = $x, y = $y)"
    case Circle(centerX, centerY, radius) => s"Circle(centerX = $centerX, centerY = $centerY, radius = $radius)"
  }


  object Origin extends Located {
    override def x: Double = 0
    override def y: Double = 0
  }

  object Bounded {
    def minimumBoundingRectangle(objects: Set[Bounded]): Bounded = ???
  }

  }

  final case class Stack[A](elements: List[A] = Nil) {
    def push(x: A): Stack[A] = Stack(x :: elements)

    def peek: Option[A] = elements.headOption

    def pop: Option[(A, Stack[A])] = peek.map(x => (x, Stack(elements.tail)))




}
