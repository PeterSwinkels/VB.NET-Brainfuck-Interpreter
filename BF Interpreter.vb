'This module's imports and settings.
Option Compare Binary
Option Explicit On
Option Infer Off
Option Strict On

Imports Microsoft.VisualBasic.ControlChars
Imports System
Imports System.Collections.Generic
Imports System.Convert
Imports System.Environment
Imports System.Globalization
Imports System.IO
Imports System.Linq
Imports System.Text

'This module contains this program's core procedures.
Public Module BFInterpreterModule

   'This procedure loads the specified code or returns any currently loaded code.
   Public Function Code(Optional CodePath As String = Nothing) As String
      Try
         Static CurrentCode As String = Nothing

         If Not CodePath = Nothing Then
            CurrentCode = Nothing
            CurrentCode = File.ReadAllText(CodePath)
         End If

         Return CurrentCode
      Catch ExceptionO As Exception
         HandleError(ExceptionO)
      End Try

      Return Nothing
   End Function

   'This procedure displays this program's information.
   Private Sub DisplayInformation()
      Try
         With My.Application.Info
            Console.WriteLine($"{ .Title} v{ .Version} - by: { .CompanyName}")
            Console.WriteLine()
            WordWrap(.Description)
            Console.WriteLine()
            Console.WriteLine()
         End With
      Catch ExceptionO As Exception
         HandleError(ExceptionO)
      End Try
   End Sub

   'This procedure converts non-displayable characters in the specified text to escape sequences.
   Public Function Escape(Text As String, Optional EscapeCharacter As Char = "/"c) As String
      Try
         Dim Escaped As New StringBuilder

         For Each Character As Char In Text.ToCharArray()
            If Character = EscapeCharacter Then
               Escaped.Append(New String(EscapeCharacter, 2))
            ElseIf Character = Tab OrElse Character >= " "c Then
               Escaped.Append(Character)
            Else
               Escaped.Append(String.Format($"{EscapeCharacter}{ToByte(Character):X2}"))
            End If
         Next Character

         Return Escaped.ToString()
      Catch ExceptionO As Exception
         HandleError(ExceptionO)
      End Try

      Return Nothing
   End Function

   'This procedure executes the specified code.
   Public Sub Execute(ExecuteCode As String, InputLineBreak As String, OutputLineBreak As String)
      Try
         Dim Character As New Char
         Dim InstructionP As Integer = &H0%
         Dim Memory As New List(Of Byte)(Enumerable.Repeat(ToByte(&H0%), &H8000%))
         Dim MemoryP As Integer = &H0%
         Dim OutputBuffer As New StringBuilder
         Dim UserInput As New Queue(Of Char)

         UserInput.Clear()
         Loops(, ExecuteCode)
         Do
            Select Case ExecuteCode.Chars(InstructionP)
               Case ">"c
                  If MemoryP = Memory.Count - &H1% Then MemoryP = &H0% Else MemoryP += &H1%
               Case "<"c
                  If MemoryP = &H0% Then MemoryP = Memory.Count - &H1% Else MemoryP -= &H1%
               Case "+"c
                  If Memory(MemoryP) = &HFF% Then Memory(MemoryP) = &H0% Else Memory(MemoryP) += ToByte(&H1%)
               Case "-"c
                  If Memory(MemoryP) = &H0% Then Memory(MemoryP) = &HFF% Else Memory(MemoryP) -= ToByte(&H1%)
               Case "."c
                  Character = ToChar(Memory(MemoryP))

                  If OutputLineBreak = Nothing Then
                     Console.Write(Escape(Character))
                  Else
                     OutputBuffer.Append(Character)

                     If Not OutputLineBreak.StartsWith(OutputBuffer.ToString()) Then
                        Console.Write(Escape(OutputBuffer.ToString()))
                        OutputBuffer.Clear()
                     ElseIf OutputBuffer.ToString() = OutputLineBreak Then
                        Console.WriteLine()
                        OutputBuffer.Clear()
                     End If
                  End If
               Case ","c
                  If UserInput.Count = 0 Then
                     Array.ForEach(GetInput().ToCharArray, AddressOf UserInput.Enqueue)
                     If Not InputLineBreak = Nothing Then Array.ForEach(InputLineBreak.ToCharArray, AddressOf UserInput.Enqueue)
                  End If
                  If UserInput.Count > 0 Then Memory(MemoryP) = ToByte(UserInput.Dequeue())
               Case "["c
                  If Memory(MemoryP) = &H0% Then InstructionP = Loops(InstructionP)
               Case "]"c
                  If Not Memory(MemoryP) = &H0% Then InstructionP = Loops(InstructionP)
            End Select

            InstructionP += &H1%
         Loop While InstructionP >= &H0% AndAlso InstructionP < ExecuteCode.Length
      Catch ExceptionO As Exception
         HandleError(ExceptionO)
      End Try
   End Sub

   'This procedure requests the user to enter input.
   Public Function GetInput(Optional Prompt As String = Nothing) As String
      Try
         Dim ErrorAt As Integer = 0
         Dim Text As String = Nothing

         Console.Write(Prompt)
         Do
            Text = Unescape(Console.ReadLine(), , ErrorAt)
            If ErrorAt > 0 Then Console.WriteLine($"Bad escape sequence at character #{ErrorAt}.")
         Loop Until ErrorAt = 0

         Return Text
      Catch ExceptionO As Exception
         HandleError(ExceptionO)
      End Try

      Return Nothing
   End Function

   'This procedure handles any errors that occur.
   Public Sub HandleError(ExceptionO As Exception)
      Try
         Console.WriteLine(ExceptionO.Message)
      Catch
         [Exit](0)
      End Try
   End Sub

   'This procedure manages the list of loop start/end addresses.
   Public Function Loops(Optional LoopInstructionP As Integer = Nothing, Optional Code As String = Nothing) As Integer
      Try
         Dim EndOfLoop As Integer = 0
         Dim LoopStack As New Stack(Of Integer)
         Dim NewInstructionP As Integer = Nothing
         Static LoopList As New Dictionary(Of Integer, Integer)

         If Code = Nothing Then
            If LoopList.Count > 0 Then NewInstructionP = LoopList.Item(LoopInstructionP)
         Else
            LoopList.Clear()

            For InstructionP As Integer = &H0% To Code.Length - &H1%
               Select Case Code.Chars(InstructionP)
                  Case "["c
                     LoopStack.Push(InstructionP)
                  Case "]"c
                     If LoopStack.Count = 0 Then
                        Console.WriteLine("End of loop without start.")
                        Exit For
                     Else
                        EndOfLoop = LoopStack.Pop()
                        LoopList.Add(EndOfLoop, InstructionP)
                        LoopList.Add(InstructionP, EndOfLoop)
                     End If
               End Select
            Next InstructionP

            If LoopStack.Count > 0 Then Console.WriteLine("Loop without end.")
         End If

         Return NewInstructionP
      Catch ExceptionO As Exception
         HandleError(ExceptionO)
      End Try

      Return Nothing
   End Function

   'This procedure is executed when this program is started.
   Public Sub Main()
      Try
         Dim InputLineBreak As String = Nothing
         Dim OutputLineBreak As String = Nothing
         Dim Path As String = Nothing

         DisplayInformation()

         If GetCommandLineArgs().GetUpperBound(0) > 0 Then
            If GetCommandLineArgs(1).Trim = "/?" Then
               WordWrap($"Usage: {My.Application.Info.AssemblyName}.exe [code path [input line break [output line break]]]")
               [Exit](0)
            Else
               Path = GetCommandLineArgs(1)
               If GetCommandLineArgs().GetUpperBound(0) > 1 Then InputLineBreak = Unescape(GetCommandLineArgs(2))
               If GetCommandLineArgs().GetUpperBound(0) > 2 Then OutputLineBreak = Unescape(GetCommandLineArgs(3))
            End If
         End If

         If Path = Nothing Then Path = GetInput("Path: ")

         If Not (Path = Nothing OrElse Code(Path) = Nothing) Then
            If InputLineBreak = Nothing Then InputLineBreak = GetInput("Input line break: ")
            If OutputLineBreak = Nothing Then OutputLineBreak = GetInput("Output line break: ")
            Execute(Code(), InputLineBreak, OutputLineBreak)
         End If
      Catch ExceptionO As Exception
         HandleError(ExceptionO)
      End Try
   End Sub

   'This procedure converts any escape sequences in the specified text to characters.
   Public Function Unescape(Text As String, Optional EscapeCharacter As Char = "/"c, Optional ByRef ErrorAt As Integer = 0) As String
      Try
         Dim Character As New Char
         Dim CharacterCode As New Integer
         Dim Index As Integer = 0
         Dim NextCharacter As New Char
         Dim Unescaped As New StringBuilder

         ErrorAt = 0

         Do Until Index >= Text.Length OrElse ErrorAt > 0
            Character = Text.Chars(Index)
            If Index < Text.Length - 1 Then NextCharacter = Text.Chars(Index + 1) Else NextCharacter = Nothing

            If Character = EscapeCharacter Then
               If NextCharacter = EscapeCharacter Then
                  Unescaped.Append(Character)
                  Index += 1
               Else
                  If NextCharacter = Nothing Then
                     ErrorAt = Index + 1
                  Else
                     If Index < Text.Length - 2 AndAlso Integer.TryParse(Text.Substring(Index + 1, 2), NumberStyles.HexNumber, Nothing, CharacterCode) Then
                        Unescaped.Append(ToChar(CharacterCode))
                        Index += 2
                     Else
                        ErrorAt = Index + 1
                     End If
                  End If
               End If
            Else
               Unescaped.Append(Character)
            End If
            Index += 1
         Loop

         Return Unescaped.ToString()
      Catch ExceptionO As Exception
         HandleError(ExceptionO)
      End Try

      Return Nothing
   End Function

   'This procedure displays word wrapped text.
   Private Sub WordWrap(Text As String)
      Try
         For Each Word As String In Text.Split(" "c)
            Console.Write($"{Word} ")
            If Console.CursorLeft + Word.Length >= Console.BufferWidth Then Console.WriteLine()
         Next Word
      Catch ExceptionO As Exception
         HandleError(ExceptionO)
      End Try
   End Sub
End Module
