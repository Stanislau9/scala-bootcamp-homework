package classesAndTraits

object ClassesAndTraits {

  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Movable {
    def move(dx: Double, dy: Double): Movable
  }

  sealed trait Shape2D extends Located with Bounded with Movable {
    def area: Double
  }

  final case class Point(x: Double, y: Double) extends Shape2D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def move(dx: Double, dy: Double): Point = Point(x + dx, y + dy)
    override def area: Double = 0
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape2D {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = centerX - radius
    override def maxX: Double = centerX + radius
    override def minY: Double = centerY - radius
    override def maxY: Double = centerY + radius
    override def move(dx: Double, dy: Double): Circle = Circle(centerX + dx, centerY + dy, radius)
    override def area: Double = math.Pi * radius * radius
  }

  final case class Rectangle(x: Double, y: Double, width: Double, height: Double) extends Shape2D {
    override def maxX: Double = x
    override def minX: Double = x - height
    override def maxY: Double = y + width
    override def minY: Double = y
    override def move(dx: Double, dy: Double): Rectangle = Rectangle(x + dx, y + dy, width, height)
    override def area: Double = width * height
  }

  final case class Triangle(a: Point, b: Point, c: Point) extends Shape2D {
    override def minX: Double = List(a.x, b.x, c.x).min
    override def maxX: Double = List(a.x, b.x, c.x).max
    override def minY: Double = List(a.y, b.y, c.y).min
    override def maxY: Double = List(a.y, b.y, c.y).max
    override def x: Double = minX
    override def y: Double = (a.x,b.x,c.x) match {
      case (x, _, _) if x == minX => a.y
      case (_, x, _) if x == minX => b.y
      case (_, _, x) if x == minX => c.y
    }
    override def move(dx: Double, dy: Double): Triangle = Triangle(a.move(dx, dy), b.move(dx, dy), c.move(dx, dy))
    override def area: Double = {
      val ab: Double = math.sqrt(math.pow(a.x - b.x, 2) + math.pow(a.y - b.y, 2))
      val ac: Double = math.sqrt(math.pow(a.x - c.x, 2) + math.pow(a.y - c.y, 2))
      val bc: Double = math.sqrt(math.pow(c.x - b.x, 2) + math.pow(c.y - b.y, 2))
      val p: Double = (ab + ac + bc) / 2
      math.sqrt(p * (p - ab) * (p - ac) * (p - bc))
    }
  }

  final case class Square(x: Double, y: Double, width: Double) extends Shape2D {
    override def maxX: Double = x
    override def minX: Double = x - width
    override def maxY: Double = y + width
    override def minY: Double = y
    override def move(dx: Double, dy: Double): Square = Square(x + dx, y + dy, width)
    override def area: Double = width * width
    }

  sealed trait Located3D {
    def x: Double
    def y: Double
    def z: Double
  }

  sealed trait Bounded3D {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Movable3D {
    def move(dx: Double, dy: Double,  dz: Double): Movable3D
  }

  sealed trait Shape3D extends Located3D with Bounded3D with Movable3D {
    def surfaceArea: Double
    def volume: Double
  }

  final case class Origin() extends Shape3D {
    override def surfaceArea: Double = 0
    override def volume: Double = 0
    override def minX: Double = 0
    override def maxX: Double = 0
    override def minY: Double = 0
    override def maxY: Double = 0
    override def minZ: Double = 0
    override def maxZ: Double = 0
    override def x: Double = 0
    override def y: Double = 0
    override def z: Double = 0
    override def move(dx: Double, dy: Double, dz: Double): Origin = Origin()
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def surfaceArea: Double = 0
    override def volume: Double = 0
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z
    override def move(dx: Double, dy: Double, dz: Double): Point3D = Point3D(x + dx, y + dy, z + dz)
  }

  final case class Shpere(centerX: Double, centerY: Double, centerZ: Double, radius: Double) extends Shape3D {
    override def surfaceArea: Double = 4 * math.Pi * radius * radius
    override def volume: Double = 4 * math.Pi * radius * radius * radius / 3
    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def minZ: Double = z - radius
    override def maxZ: Double = z + radius
    override def move(dx: Double, dy: Double, dz: Double): Shpere = Shpere(x + dx, y + dy, z + dz, radius)
  }

  final case class Cube(x: Double, y: Double, z: Double, width: Double) extends Shape3D {
    override def surfaceArea: Double = width * width * 6
    override def volume: Double = width * width * width
    override def minX: Double = x
    override def maxX: Double = x + width
    override def minY: Double = y - width
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z + width
    override def move(dx: Double, dy: Double, dz: Double): Cube = Cube(x + dx, y + dy, z + dz, width)
  }

  final case class Cuboid(x: Double, y: Double, z: Double, length: Double, height: Double, width: Double) extends Shape3D {
    override def surfaceArea: Double = (length * width + length * height + height * width) * 2
    override def volume: Double = length * height * width
    override def minX: Double = x
    override def maxX: Double = x + length
    override def minY: Double = y - height
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z + width
    override def move(dx: Double, dy: Double, dz: Double): Cuboid = Cuboid(x + dx, y + dy, z + dz, length, height, width)
  }

  final case class Triangle3D(a: Point3D, b: Point3D, c: Point3D) extends Shape3D {
    override def surfaceArea: Double = ???
    override def volume: Double = 0
    override def x: Double = minX
    override def y: Double = (a.x, b.x, c.x)match {
      case (x, _, _) if x == minX => a.y
      case (_, x, _) if x == minX => b.y
      case (_, _, x) if x == minX => c.y
    }
    override def z: Double = (a.x, b.x, c.x)match {
      case (x, _, _) if x == minX => a.z
      case (_, x, _) if x == minX => b.z
      case (_, _, x) if x == minX => c.z
    }
    override def minX: Double = List(a.x, b.x, c.x).min
    override def maxX: Double = List(a.x, b.x, c.x).max
    override def minY: Double = List(a.y, b.y, c.y).min
    override def maxY: Double = List(a.y, b.y, c.y).max
    override def minZ: Double = List(a.z, b.z, c.z).min
    override def maxZ: Double = List(a.z, b.z, c.z).max
    override def move(dx: Double, dy: Double, dz: Double): Triangle3D =
      Triangle3D(a.move(dx, dy, dz), b.move(dx, dy, dz), c.move(dx, dy, dz))
  }
}
