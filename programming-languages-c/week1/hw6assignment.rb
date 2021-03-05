# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # Class method to choose the next piece
  # Class methods are declared by binding the method to self
  def self.next_piece(board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  # Class array holding all the pieces and their rotations including the extra
  # three pieces.
  All_My_Pieces = [
    [
      [[0, 0], [1, 0], [0, 1], [1, 1]] # square (only needs one)
    ],
    rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
    [
      [[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
      [[0, 0], [0, -1], [0, 1], [0, 2]]
    ],
    rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
    rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
    rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
    rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
    [
      [[0, 0], [-1, 0], [1, 0], [-2, 0], [2, 0]], # extra long (only needs two)
      [[0, 0], [0, -1], [0, 1], [0, -2], [0, 2]]
    ],
    rotations([[0, 0], [-1, 0], [-1, 1], [0, 1], [1, 1]]), # b or q ?
    rotations([[0, 0], [0, 1], [1, 1]]) # Small L ?
  ]
end

class MyBoard < Board
  def initialize(game)
    super
    @current_block = MyPiece.next_piece(self)
    @cheating = false
  end

  def rotate_180_degree
    if !game_over? && @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def cheat
    if @score > 100 && !@cheating
      @score -= 100
      @cheating = true
    end
  end

  # Gets the next piece
  def next_piece
    if !@cheating
      @current_block = MyPiece.next_piece(self)
    else
      @current_block = MyPiece.new([[[0, 0]]], self)
      @cheating = false
    end
    @current_pos = nil
  end

  # This method needs to be overriden as in the default implementation, the
  # number of blocks a piece is made of was a fixed number (4) but now we have
  # pieces made up of 5 and 3 blocks as well.
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.length - 1).each{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  # Creates a canvas and the board that interacts with it
  # This method was overriden to initiate MyBoard instead of Board
  # We cannot use super in here as with the super call, the board will be
  # drawn before we have the chance to override the @board instance variable.
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(
      @board.block_size * @board.num_rows + 3,
      @board.block_size * @board.num_columns + 6, 24, 80
    )
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc { @board.rotate_180_degree })
    @root.bind('c', proc { @board.cheat })
  end
end
