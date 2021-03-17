# University of Washington, Programming Languages, Homework 7 Tests

require 'test-unit'
require './hw7'

# NOTE:
# All intersection tests are carried out directly from the Intersect class,
# thus testing double dispatch along with the appropriate answer.

class TestPoint < Test::Unit::TestCase
  def setup
    @p = Point.new(3, 4)
  end

  def test_initialization
    assert_equal(@p.x, 3)
    assert_equal(@p.y, 4)
  end

  def test_preprocess_prog
    output = @p.preprocess_prog
    assert_equal(@p, output)
  end

  def test_eval
    output = @p.eval_prog([])
    assert_equal(@p, output)
  end

  def test_shift
    output = @p.shift(1, 1)
    assert_equal(output.x, 4)
    assert_equal(output.y, 5)
  end
end


class TestLine < Test::Unit::TestCase
  def setup
    @l = Line.new(3, 4)
  end

  def test_initialization
    assert_equal(@l.m, 3)
    assert_equal(@l.b, 4)
  end

  def test_preprocess_prog
    output = @l.preprocess_prog
    assert_equal(@l, output)
  end

  def test_eval
    output = @l.eval_prog([])
    assert_equal(@l, output)
  end

  def test_shift
    output = @l.shift(1, 1)
    assert_equal(output.m, 3)
    assert_equal(output.b, 2)
  end
end


class TestVerticalLine < Test::Unit::TestCase
  def setup
    @vl = VerticalLine.new(3)
  end

  def test_initialization
    assert_equal(@vl.x, 3)
  end

  def test_preprocess_prog
    output = @vl.preprocess_prog
    assert_equal(@vl, output)
  end

  def test_eval
    output = @vl.eval_prog([])
    assert_equal(@vl, output)
  end

  def test_shift
    output = @vl.shift(1, 1)
    assert_equal(output.x, 4)
  end
end


class TestLineSegment < Test::Unit::TestCase
  def test_initialization
    ls = LineSegment.new(1, 2, 3, 4)
    assert_equal(ls.x1, 1)
    assert_equal(ls.y1, 2)
    assert_equal(ls.x2, 3)
    assert_equal(ls.y2, 4)
  end

  def test_preprocess_prog1
    l1 = LineSegment.new(1, 2, 1, 2)
    output = l1.preprocess_prog
    assert_instance_of(Point, output)
    assert_equal(l1.x1, output.x)
    assert_equal(l1.y1, output.y)
  end

  def test_preprocess_prog2
    l1 = LineSegment.new(1, 2, 1, 3)
    output = l1.preprocess_prog
    assert_equal(l1, output)
  end

  def test_preprocess_prog3
    l1 = LineSegment.new(1, 3, 1, 2)
    output = l1.preprocess_prog
    assert_equal(l1.x1, output.x2)
    assert_equal(l1.y1, output.y2)
    assert_equal(l1.x2, output.x1)
    assert_equal(l1.y2, output.y1)
  end

  def test_preprocess_prog4
    l1 = LineSegment.new(1, 0, 2, 0)
    output = l1.preprocess_prog
    assert_equal(l1, output)
  end

  def test_preprocess_prog5
    l1 = LineSegment.new(2, 0, 1, 0)
    output = l1.preprocess_prog
    assert_equal(l1.x1, output.x2)
    assert_equal(l1.y1, output.y2)
    assert_equal(l1.x2, output.x1)
    assert_equal(l1.y2, output.y1)
  end

  def test_eval
    ls = LineSegment.new(1, 2, 3, 4)
    output = ls.eval_prog([])
    assert_equal(ls, output)
  end

  def test_shift
    ls = LineSegment.new(1, 2, 3, 4)
    output = ls.shift(1, 1)
    assert_equal(output.x1, 2)
    assert_equal(output.y1, 3)
    assert_equal(output.x2, 4)
    assert_equal(output.y2, 5)
  end
end


class TestIntersect < Test::Unit::TestCase
  def test_point_with_point1
    i = Intersect.new(Point.new(1, 2), Point.new(1, 2))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(Point, output)
    assert_equal(output.x, 1)
    assert_equal(output.y, 2)
  end

  def test_point_with_point2
    i = Intersect.new(Point.new(1, 2), Point.new(1, 3))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(NoPoints, output)
  end

  def test_point_with_line1
    i = Intersect.new(Point.new(2, 10), Line.new(4, 2))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(Point, output)
    assert_equal(output.x, 2)
    assert_equal(output.y, 10)
  end

  def test_point_with_line2
    i = Intersect.new(Point.new(3, 9), Line.new(4, 2))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(NoPoints, output)
  end

  def test_point_with_vertical_line1
    i = Intersect.new(Point.new(4, 2), VerticalLine.new(4))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(Point, output)
    assert_equal(output.x, 4)
    assert_equal(output.y, 2)
  end

  def test_point_with_vertical_line2
    i = Intersect.new(Point.new(2, 3), VerticalLine.new(3))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(NoPoints, output)
  end

  def test_line_with_point1
    i = Intersect.new(Line.new(4, 2), Point.new(2, 10))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(Point, output)
    assert_equal(output.x, 2)
    assert_equal(output.y, 10)
  end

  def test_line_with_point2
    i = Intersect.new(Line.new(4, 2), Point.new(3, 9))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(NoPoints, output)
  end

  def test_line_with_line1
    i = Intersect.new(Line.new(2, 4), Line.new(2, 3))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(NoPoints, output)
  end

  def test_line_with_line2
    i = Intersect.new(Line.new(2, 4), Line.new(2, 4))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(Line, output)
    assert_equal(output.m, 2)
    assert_equal(output.b, 4)
  end

  def test_line_with_line3
    i = Intersect.new(Line.new(2, 4), Line.new(4, 6))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(Point, output)
    assert_equal(output.x, -1)
    assert_equal(output.y, 2)
  end

  def test_line_with_vertical_line
    i = Intersect.new(Line.new(1, 6), VerticalLine.new(4))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(Point, output)
    assert_equal(output.x, 4)
    assert_equal(output.y, 10)
  end

  def test_vertical_line_with_point1
    i = Intersect.new(VerticalLine.new(4), Point.new(4, 2))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(Point, output)
    assert_equal(output.x, 4)
    assert_equal(output.y, 2)
  end

  def test_vertical_line_with_point2
    i = Intersect.new(VerticalLine.new(3), Point.new(2, 3))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(NoPoints, output)
  end

  def test_vertical_line_with_line
    i = Intersect.new(VerticalLine.new(4), Line.new(1, 6))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(Point, output)
    assert_equal(output.x, 4)
    assert_equal(output.y, 10)
  end

  def test_vertical_line_with_line1
    i = Intersect.new(VerticalLine.new(4), VerticalLine.new(4))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(VerticalLine, output)
    assert_equal(output.x, 4)
  end

  def test_vertical_line_with_line2
    i = Intersect.new(VerticalLine.new(6), VerticalLine.new(4))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(NoPoints, output)
  end

  def test_line_segment_with_point1
    i = Intersect.new(LineSegment.new(0, 0, 10, 10), Point.new(4, 4))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(Point, output)
    assert_equal(output.x, 4)
    assert_equal(output.y, 4)
  end

  def test_line_segment_with_point2
    i = Intersect.new(LineSegment.new(0, 0, 10, 10), Point.new(4, 6))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(NoPoints, output)
  end

  def test_line_segment_with_line1
    i = Intersect.new(LineSegment.new(0, 0, 10, 10), Line.new(1, 4))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(NoPoints, output)
  end

  def test_line_segment_with_line2
    i = Intersect.new(LineSegment.new(0, 0, 10, 10), Line.new(0.4, 3))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(Point, output)
    assert_equal(output.x, 5)
    assert_equal(output.y, 5)
  end

  def test_line_segment_with_line3
    i = Intersect.new(LineSegment.new(0, 0, 10, 10), Line.new(1, 0))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(LineSegment, output)
    assert_equal(output.x1, 0)
    assert_equal(output.y1, 0)
    assert_equal(output.x2, 10)
    assert_equal(output.y2, 10)
  end

  def test_line_segment_with_vertical_line1
    i = Intersect.new(LineSegment.new(0, 0, 2, 2), VerticalLine.new(4))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(NoPoints, output)
  end

  def test_line_segment_with_vertical_line2
    i = Intersect.new(LineSegment.new(0, 0, 3, 3), VerticalLine.new(3))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(Point, output)
    assert_equal(output.x, 3)
    assert_equal(output.y, 3)
  end

  def test_point_with_line_segment1
    i = Intersect.new(Point.new(4, 4), LineSegment.new(0, 0, 10, 10))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(Point, output)
    assert_equal(output.x, 4)
    assert_equal(output.y, 4)
  end

  def test_point_with_line_segment2
    i = Intersect.new(Point.new(4, 6), LineSegment.new(0, 0, 10, 10))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(NoPoints, output)
  end

  def test_line_with_line_segment1
    i = Intersect.new(Line.new(1, 4), LineSegment.new(0, 0, 10, 10))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(NoPoints, output)
  end

  def test_line_with_line_segment2
    i = Intersect.new(Line.new(0.4, 3), LineSegment.new(0, 0, 10, 10))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(Point, output)
    assert_equal(output.x, 5)
    assert_equal(output.y, 5)
  end

  def test_line_with_line_segment3
    i = Intersect.new(Line.new(1, 0), LineSegment.new(0, 0, 10, 10))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(LineSegment, output)
    assert_equal(output.x1, 0)
    assert_equal(output.y1, 0)
    assert_equal(output.x2, 10)
    assert_equal(output.y2, 10)
  end

  def test_vertical_line_with_line_segment1
    i = Intersect.new(VerticalLine.new(4), LineSegment.new(0, 0, 2, 2))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(NoPoints, output)
  end

  def test_vertical_line_with_line_segment2
    i = Intersect.new(VerticalLine.new(3), LineSegment.new(0, 0, 3, 3))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(Point, output)
    assert_equal(output.x, 3)
    assert_equal(output.y, 3)
  end

  def test_line_segment_with_line_segment1
    i = Intersect.new(LineSegment.new(0, 0, 5, 5), LineSegment.new(5, 0, 0, 5))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(Point, output)
    assert_equal(output.x, 2.5)
    assert_equal(output.y, 2.5)
  end

  def test_line_segment_with_line_segment2
    i = Intersect.new(LineSegment.new(0, 0, 5, 5), LineSegment.new(6, 6, 9, 9))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(NoPoints, output)
  end

  def test_line_segment_with_line_segment3
    i = Intersect.new(LineSegment.new(0, 0, 5, 5), LineSegment.new(1, 1, 3, 3))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(LineSegment, output)
    assert_equal(output.x1, 1)
    assert_equal(output.y1, 1)
    assert_equal(output.x2, 3)
    assert_equal(output.y2, 3)
  end

  def test_line_segment_with_line_segment4
    i = Intersect.new(LineSegment.new(0, 0, 5, 5), LineSegment.new(9, 4, 5, 5))
    output = i.preprocess_prog.eval_prog([])
    assert_instance_of(Point, output)
    assert_equal(output.x, 5)
    assert_equal(output.y, 5)
  end
end


class TestVar < Test::Unit::TestCase
  def test_preprocess_prog
    v = Var.new("a")
    output = v.preprocess_prog
    assert_equal(v, output)
  end

  def test_eval
    v = Var.new("a")
    output = v.eval_prog([["a", Point.new(1, 2)]])
    assert_instance_of(Point, output)
    assert_equal(output.x, 1)
    assert_equal(output.y, 2)
  end
end

class TestLet < Test::Unit::TestCase
  def test_eval1
    l = Let.new(
      "a",
      LineSegment.new(-1, -2, 3, 4),
      Intersect.new(Var.new("a"), LineSegment.new(3, 4, -1, -2)))
    output = l.preprocess_prog.eval_prog([])
    assert_instance_of(LineSegment, output)
    assert_equal(output.x1, -1)
    assert_equal(output.y1, -2)
    assert_equal(output.x2, 3)
    assert_equal(output.y2, 4)
  end

  def test_eval2
    l = Let.new(
      "a",
      LineSegment.new(-1, -2, 3, 4),
      Let.new("b",
              LineSegment.new(3, 4, -1, -2),
              Intersect.new(Var.new("a"), Var.new("b"))))
    output = l.preprocess_prog.eval_prog([])
    assert_instance_of(LineSegment, output)
    assert_equal(output.x1, -1)
    assert_equal(output.y1, -2)
    assert_equal(output.x2, 3)
    assert_equal(output.y2, 4)
  end
end


class TestShift < Test::Unit::TestCase
  def test_eval
    s = Shift.new(3, 5, LineSegment.new(-1, -2, 3, 4))
    output = s.preprocess_prog.eval_prog([])
    assert_equal(output.x1, 2)
    assert_equal(output.y1, 3)
    assert_equal(output.x2, 6)
    assert_equal(output.y2, 9)
  end
end
