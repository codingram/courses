# University of Washington, Programming Languages, Homework 7, hw7.rb
# (See also ML code)

# a little language for 2D geometry objects

# each subclass of GeometryExpression, including subclasses of GeometryValue,
#  needs to respond to messages preprocess_prog and eval_prog
#
# each subclass of GeometryValue additionally needs:
#   * shift
#   * intersect, which uses the double-dispatch pattern
#   * intersectPoint, intersectLine, and intersectVerticalLine for
#       for being called by intersect of appropriate clases and doing
#       the correct intersection calculuation
#   * (We would need intersectNoPoints and intersectLineSegment, but these
#      are provided by GeometryValue and should not be overridden.)
#   *  intersectWithSegmentAsLineResult, which is used by
#      intersectLineSegment as described in the assignment
#
# you can define other helper methods, but will not find much need to

# Note: geometry objects should be immutable: assign to fields only during
#       object construction

# Note: For eval_prog, represent environments as arrays of 2-element arrays
# as described in the assignment

class GeometryExpression
  # do *not* change this class definition
  # Class constant which can be accessed with ::
  Epsilon = 0.00001
end

class GeometryValue
  # do *not* change methods in this class definition
  # you can add methods if you wish

  private
  # some helper methods that may be generally useful
  def real_close(r1,r2)
    (r1 - r2).abs < GeometryExpression::Epsilon
  end

  def real_close_point(x1,y1,x2,y2)
    real_close(x1,x2) && real_close(y1,y2)
  end

  # two_points_to_line could return a Line or a VerticalLine
  def two_points_to_line(x1,y1,x2,y2)
    if real_close(x1,x2)
      VerticalLine.new x1
    else
      m = (y2 - y1).to_f / (x2 - x1)
      b = y1 - m * x1
      Line.new(m,b)
    end
  end

  public
  # we put this in this class so all subclasses can inherit it:
  # the intersection of self with a NoPoints is a NoPoints object
  def intersectNoPoints np
    np  # could also have NoPoints.new here instead
  end

  # we put this in this class so all subclasses can inhert it:
  # the intersection of self with a LineSegment is computed by
  # first intersecting with the line containing the segment and then
  # calling the result's intersectWithSegmentAsLineResult with the segment
  def intersectLineSegment seg
    line_result = intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2))
    line_result.intersectWithSegmentAsLineResult seg
  end
end

class NoPoints < GeometryValue
  # do *not* change this class definition: everything is done for you
  # (although this is the easiest class, it shows what methods every subclass
  # of geometry values needs)
  # However, you *may* move methods from here to a superclass if you wish to

  # Note: no initialize method only because there is nothing it needs to do
  # All values evaluate to self
  def eval_prog env
    self
  end

  # no pre-processing to do here
  def preprocess_prog
    self
  end

  # shifting no-points is no-points
  def shift(dx, dy)
    self
  end

  # will be NoPoints but follow double-dispatch
  def intersect other
    other.intersectNoPoints self
  end

  # intersection with point and no-points is no-points
  def intersectPoint p
    self
  end

  # intersection with line and no-points is no-points
  def intersectLine line
    self
  end

  # intersection with line and no-points is no-points
  def intersectVerticalLine vline
    self
  end

  # if self is the intersection of (1) some shape s and (2)
  # the line containing seg, then we return the intersection of the
  # shape s and the seg.  seg is an instance of LineSegment
  def intersectWithSegmentAsLineResult seg
    self
  end
end


class Point < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods

  # Note: You may want a private helper method like the local
  # helper function inbetween in the ML code
  attr_reader :x, :y

  def initialize(x,y)
    @x = x
    @y = y
  end

  def preprocess_prog
    self
  end

  def eval_prog env
    self
  end

  def shift(dx, dy)
    Point.new(x + dx, y + dy)
  end

  def intersect other
    other.intersectPoint self
  end

  # We can even return p in place of self as both will be the same points.
  # Point x Point
  def intersectPoint p
    return self if real_close_point(x, y, p.x, p.y)
    NoPoints.new
  end

  # Line x Point
  def intersectLine line
    intersect(line)
  end

  # VerticalLine x Point
  def intersectVerticalLine vline
    intersect(vline)
  end

  def intersectWithSegmentAsLineResult seg
    return self if inbetween(x, seg.x1, seg.x2) and inbetween(y, seg.y1, seg.y2)
    NoPoints.new
  end

  private

  def inbetween(v, end1, end2)
    epsilon = GeometryExpression::Epsilon
    (end1 - epsilon <= v and v <= end2 + epsilon) or
      (end2 - epsilon <= v and v <= end1 + epsilon)
  end
end


class Line < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :m, :b

  def initialize(m,b)
    @m = m
    @b = b
  end

  def preprocess_prog
    self
  end

  def eval_prog env
    self
  end

  def shift(dx, dy)
    Line.new(m, b + dy - m * dx)
  end

  def intersect other
    other.intersectLine self
  end

  # Point x Line(self)
  def intersectPoint p
    return p if real_close(p.y, m * p.x + b)
    NoPoints.new
  end

  # Line x Line(self)
  def intersectLine line
    if real_close(line.m, m)
      return self if real_close(line.b, b)
      NoPoints.new
    else
      x = (b - line.b) / (line.m - m)
      y = line.m * x + line.b
      Point.new(x, y)
    end
  end

  # VerticalLine x Line(self)
  def intersectVerticalLine vline
    intersect(vline)
  end

  def intersectWithSegmentAsLineResult seg
    seg
  end
end


class VerticalLine < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :x

  def initialize x
    @x = x
  end

  def preprocess_prog
    self
  end

  def eval_prog env
    self
  end

  def shift(dx, dy)
    VerticalLine.new(x + dx)
  end

  def intersect other
    other.intersectVerticalLine self
  end

  # Point x VerticalLine
  def intersectPoint p
    return p if real_close(x, p.x)
    NoPoints.new
  end

  # Line x VerticalLine
  def intersectLine line
    Point.new(x, line.m * x + line.b)
  end

  # VerticalLine x VerticalLine
  def intersectVerticalLine vline
    return self if real_close(x, vline.x)
    NoPoints.new
  end

  def intersectWithSegmentAsLineResult seg
    seg
  end
end


class LineSegment < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: This is the most difficult class.  In the sample solution,
  #  preprocess_prog is about 15 lines long and
  # intersectWithSegmentAsLineResult is about 40 lines long
  attr_reader :x1, :y1, :x2, :y2
  def initialize (x1,y1,x2,y2)
    @x1 = x1
    @y1 = y1
    @x2 = x2
    @y2 = y2
  end

  def preprocess_prog
    # If both endpoints are real close then return a point.
    if real_close_point(x1, y1, x2, y2)
      return Point.new(x1, y1)
    # If the x coordinates are real close then switch points if y coordinates
    # are in the reverse order
    elsif real_close(x1, x2)
      return LineSegment.new(x2, y2, x1, y1) if y1 > y2
    # If the x coordinates are not close, then we will check the x coordinates
    # and switch the points accordingly.
    else
      return LineSegment.new(x2, y2, x1, y1) if x1 > x2
    end
    # At the end, if none of the above conditions hold true, return self as all
    # checks were successful.
    self
  end

  def eval_prog env
    self
  end

  def shift(dx, dy)
    LineSegment.new(x1 + dx, y1 + dy, x2 + dx, y2 + dy)
  end

  def intersect other
    other.intersectLineSegment self
  end

  def intersectPoint p
    intersect(p)
  end

  def intersectLine line
    intersect(line)
  end

  def intersectVerticalLine vline
    intersect(vline)
  end

  def intersectWithSegmentAsLineResult seg
    if real_close(seg.x1, seg.x2)
      sega, segb = if seg.y1 < y1 then [self, seg] else [seg, self] end
      if real_close(sega.y2, segb.y1)
        Point.new(sega.x2, sega.y2)
      elsif sega.y2 < segb.y1
        NoPoints.new
      elsif sega.y2 > segb.y2
        LineSegment.new(segb.x1, segb.y1, segb.x2, segb.y2)
      else
        LineSegment.new(segb.x1, segb.y1, sega.x2, sega.y2)
      end
    else
      sega, segb = if x1 < seg.x1 then [self, seg] else [seg, self] end
      if real_close(sega.x2, segb.x1)
        Point.new(sega.x2, sega.y2)
      elsif sega.x2 < segb.x1
        NoPoints.new
      elsif sega.x2 > segb.x2
        LineSegment.new(segb.x1, segb.y1, segb.x2, segb.y2)
      else
        LineSegment.new(segb.x1, segb.y1, sega.x2, sega.y2)
      end
    end
  end
end

# Note: there is no need for getter methods for the non-value classes

class Intersect < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(e1,e2)
    @e1 = e1
    @e2 = e2
  end

  def preprocess_prog
    Intersect.new(@e1.preprocess_prog, @e2.preprocess_prog)
  end

  def eval_prog env
    @e1.eval_prog(env).intersect(@e2.eval_prog(env))
  end
end

class Let < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: Look at Var to guide how you implement Let
  def initialize(s,e1,e2)
    @s = s
    @e1 = e1
    @e2 = e2
  end

  def preprocess_prog
    Let.new(@s, @e1.preprocess_prog, @e2.preprocess_prog)
  end

  def eval_prog env
    new_env = Array.new(env)
    new_env.insert(0, [@s, @e1.eval_prog(env)])
    @e2.eval_prog new_env
  end
end

class Var < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize s
    @s = s
  end

  def preprocess_prog
    self
  end

  def eval_prog env # remember: do not change this method
    pr = env.assoc @s
    raise "undefined variable" if pr.nil?
    pr[1]
  end
end

class Shift < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(dx,dy,e)
    @dx = dx
    @dy = dy
    @e = e
  end

  def preprocess_prog
    Shift.new(@dx, @dy, @e.preprocess_prog)
  end

  def eval_prog env
    @e.eval_prog(env).shift(@dx, @dy)
  end
end
