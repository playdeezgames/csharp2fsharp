using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Reflection;
using System.Windows.Forms;

namespace csharpversion
{
    //This code accompanies a blog post found at https://csharptofsharp.wordpress.com/2015/12/13/and-then-i-made-a-windows-app-jetlag/
    //It primarily exists to show a contrast with the F# code that does exactly the same thing
    class Program
    {
        static readonly Size screenSize = new Size(640, 480);
        static readonly Size boardSize = new Size(40, 30);
        static readonly Size cellSize = new Size(screenSize.Width / boardSize.Width, screenSize.Height / boardSize.Height);
        const int tailLength = 6;

        enum GameState
        {
            Title,
            Play
        }
        enum Direction
        {
            Left,
            Straight,
            Right
        }

        static int directionDelta(Direction direction)
        {
            switch (direction)
            {
                case Direction.Left:
                    return -1;
                case Direction.Right:
                    return 1;
                default:
                    return 0;
            }
        }

        static Queue<int> createIntList(int count, int value)
        {
            Queue<int> result = new Queue<int>();
            while (result.Count() < count)
            {
                result.Enqueue(value);
            }
            return result;
        }

        static void drawBricks(Graphics g, Brush brush, int row, Queue<int> bricks)
        {
            foreach (int head in bricks)
            {
                g.FillRectangle(brush, new Rectangle(cellSize.Width * head, cellSize.Height * row, cellSize.Width, cellSize.Height));
                row++;
            }
        }

        static void drawTail(Graphics g, Brush tailBrush, Brush headBrush, int row, Queue<int> tail)
        {
            foreach (int head in tail)
            {
                Brush brush = tailBrush;
                if (row == tailLength - 1)
                {
                    brush = headBrush;
                }
                g.FillRectangle(brush, new Rectangle(cellSize.Width * head, cellSize.Height * row, cellSize.Width, cellSize.Height));
                row++;
            }
        }

        static Form gameWindow = new Form();

        static void scroll(int value, Queue<int> items)
        {
            items.Enqueue(value);
            items.Dequeue();
        }

        static int calculateScore(int runSize)
        {
            return (runSize * (runSize + 1)) / 2;
        }

        class GameData
        {
            GameState gameState = GameState.Title;
            int score = 0;
            Queue<int> blocks = createIntList(boardSize.Height, 0);
            Queue<int> tail = createIntList(tailLength, boardSize.Width / 2);
            int currentRun = 0;
            Direction direction = Direction.Straight;

            static readonly Brush wallBrush = new SolidBrush(Color.Blue);
            static readonly Brush tailBrush = new SolidBrush(Color.Orange);
            static readonly Brush headBrush = new SolidBrush(Color.Red);
            static readonly Brush blockBrush = new SolidBrush(Color.White);
            static readonly Brush fontBrush = new SolidBrush(Color.LightGreen);
            static readonly Rectangle leftWall = new Rectangle(0, 0, cellSize.Width, screenSize.Height);
            static readonly Rectangle rightWall = new Rectangle(screenSize.Width - cellSize.Width, 0, cellSize.Width, screenSize.Height);
            static readonly Font font = new Font("Tahoma", cellSize.Height);
            static readonly Random random = new Random();
            const string gameOverText = "Press Space";

            public void HandleKey(Keys keyCode)
            {
                switch (gameState)
                {
                    case GameState.Title:
                        if (keyCode == Keys.Space)
                        {
                            Reset();
                            gameState = GameState.Play;
                        }
                        break;
                    default:
                        switch (keyCode)
                        {
                            case Keys.Left:
                                if (direction == Direction.Right)
                                {
                                    score += calculateScore(currentRun);
                                }
                                direction = Direction.Left;
                                currentRun = 0;
                                break;
                            case Keys.Right:
                                if (direction == Direction.Left)
                                {
                                    score += calculateScore(currentRun);
                                }
                                direction = Direction.Right;
                                currentRun = 0;
                                break;
                        }
                        break;
                }
            }

            public void HandlePaint(Graphics g)
            {
                g.Clear(Color.Black);
                drawBricks(g, blockBrush, 0, blocks);
                g.FillRectangle(wallBrush, leftWall);
                g.FillRectangle(wallBrush, rightWall);
                drawTail(g, tailBrush, headBrush, 0, tail);
                g.DrawString(score.ToString(), font, fontBrush, new PointF(cellSize.Width, 0.0f));
                if (gameState == GameState.Title)
                {
                    var gameOverSize = g.MeasureString(gameOverText, font);
                    g.DrawString(gameOverText, font, fontBrush, new PointF(screenSize.Width / 2.0f - gameOverSize.Width / 2.0f, screenSize.Height / 2.0f - gameOverSize.Height / 2.0f));
                }
            }

            public void HandleTick()
            {
                if (gameState == GameState.Play)
                {
                    int nextBlockColumn = random.Next(boardSize.Width - 2) + 1;
                    scroll(nextBlockColumn, blocks);
                    int nextTailColumn = tail.Last() + directionDelta(direction);
                    scroll(nextTailColumn, tail);
                    currentRun++;
                    gameWindow.Invalidate();
                    if (tail.Last() == 0 || tail.Last() == boardSize.Width - 1 || tail.Last() == blocks.ToList()[tailLength - 1])
                    {
                        gameState = GameState.Title;
                    }
                }
            }

            public void Reset()
            {
                blocks = createIntList(boardSize.Height, 0);
                tail = createIntList(tailLength, boardSize.Width / 2);
                score = 0;
                gameState = GameState.Title;
                direction = Direction.Straight;
                gameWindow.Invalidate();
            }


        }

        static GameData gameData = new GameData();

        static Timer timer = new Timer();

        [STAThread]
        static void Main()
        {
            gameWindow.ClientSize = screenSize;
            gameWindow.FormBorderStyle = FormBorderStyle.FixedSingle;
            gameWindow.MaximizeBox = false;
            gameWindow.MinimizeBox = false;
            gameWindow.GetType().GetProperty("DoubleBuffered", BindingFlags.Instance | BindingFlags.NonPublic).SetValue(gameWindow, true, null);
            gameWindow.Text = "C# JetLag";
            gameWindow.KeyDown += (_, e) => gameData.HandleKey(e.KeyCode);
            gameWindow.Paint += (_, g) => gameData.HandlePaint(g.Graphics);

            timer.Interval = 100;
            timer.Tick += (_1, _2) => gameData.HandleTick();
            timer.Start();

            Application.Run(gameWindow);
        }
    }
}