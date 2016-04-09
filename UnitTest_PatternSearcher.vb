' <summary>
' This is a test class for project UnitTestPatternSearcher
' and illustrated in ClassDiagram_PatternSearcher.cd
' The tests work with classes: PatternSearcher, SearchResults, and FoundResults
Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports System.IO

<TestClass()> Public Class UnitTest_PatternSearcher

    Private _FileType As String
    Private _IsCaseSensitive As Boolean
    Private _Pattern As String
    Private _StartingLocation As String
    Private filename As String
    Private input As String
    Private line As String
    Private dictionaryContainer As Dictionary(Of String, Integer)

#Region "Unit Tests"
    <TestInitialize()> _
    Public Sub Initialize()
        _FileType = "*.txt"
        _IsCaseSensitive = True
        _Pattern = "html"
        _StartingLocation = "..\..\"
        filename = "test.txt"
        input = "Hello World"
        line = "No, Sam I am.  I do not like green eggs and ham."
        dictionaryContainer = New Dictionary(Of String, Integer)()
    End Sub

    <TestMethod()> _
    Public Sub Validate_Starting_Location()
        'Dim isValidDirectory As Boolean = Directory.Exists(_StartingLocation)
        'Testing function call to ValidateStartingLocation(_StartingLocation)
        Dim isValidDirectory As Boolean = ValidateStartingLocation(_StartingLocation)
        If isValidDirectory Then
            Dim fullPath As String = New DirectoryInfo(_StartingLocation).FullName.ToString()
            Console.WriteLine("The path: '" & _StartingLocation & "' correctly identifies the full directory:")
            Console.WriteLine(fullPath)
        End If
        Assert.IsTrue(isValidDirectory, "The directory '" & _StartingLocation & "' should exist.")
    End Sub

    <TestMethod()> _
    Public Sub Extract_Filename_Extension_From_String()
        Dim extractedExtension As String = ""
        Dim dotIndex As Integer

        If filename <> Nothing And filename.Contains(".") Then
            dotIndex = filename.LastIndexOf(".")
            extractedExtension = filename.Substring(dotIndex, filename.Length - dotIndex)
            Console.WriteLine("The filename '" & filename & "' has the extension '" & extractedExtension & "'")
            'Testing function call to ValidateFileType(extractedExtension)
            If ValidateFileType(extractedExtension) Then
                Console.WriteLine("The extension '" & extractedExtension & "' is a valid file type.")
            Else
                Console.WriteLine("The extension '" & extractedExtension & "' is not a valid file type.")
            End If
        Else
            Console.WriteLine("The extension '" & extractedExtension & "' is not a valid file type.")
        End If
    End Sub

    <TestMethod()> _
    Public Sub Validate_FileType()
        Dim isValidExtension As Boolean = False
        Dim validExtensions As IEnumerable(Of String) = {".*", ".txt", ".html", ".xml"}

        isValidExtension = validExtensions.Any(Function(x) x = _FileType)
        'Assert.IsFalse(isValidExtension, "The file type '" & _FileType & "' is a valid file extension")
        Assert.IsTrue(isValidExtension, "The file type '" & _FileType & "' is not a valid file extension")
    End Sub

    <TestMethod()> _
    Public Sub Validate_File_Exists()
        'Public Sub Validate_File_Exists(ByVal directory As String, ByVal filename As String)
        Dim isFileFound As Boolean = False
        'Comment out six lines for testing function call to ValidateFileExists(filename)
        'Dim di As New IO.DirectoryInfo(_StartingLocation)
        'Dim aryFi As IO.FileInfo() = di.GetFiles(filename)
        'Dim fi As IO.FileInfo
        'For Each fi In aryFi
        'isFileFound = True
        'Next
        isFileFound = ValidateFileExists(filename)
        'Assert.IsFalse(isFileFound, "The file(s) identified by '" & filename & "' were found.")
        Assert.IsTrue(isFileFound, "The file(s) identified by '" & filename & "' were not found.")
    End Sub

    <TestMethod()> _
    Public Sub Validate_Pattern()
        Dim isValidPattern As Boolean = False
        Dim patternLength As Integer = _Pattern.Length
        'Assert.IsTrue(patternLength > 0, "The patterm '" & _Pattern & "' should not be empty.")
        Assert.IsTrue(ValidatePattern(_Pattern), "The pattern '" & _Pattern & "' should not be empty.")
    End Sub

    <TestMethod()> _
    Public Sub Validate_Single_File()
        'This test only works for a single file 
        'This test fails when a wildcard symbol is used to locate multiple files.
        Dim isValidFile As Boolean = File.Exists(_StartingLocation & filename)
        'Dim isValidFile As Boolean = ValidateSingleFile(_StartingLocation & filename)
        Assert.IsTrue(isValidFile, "The file '" & _StartingLocation & filename & "' should exist.")
    End Sub

    <TestMethod()> _
    Public Sub Does_The_Token_End_With_A_Symbol()
        Dim token As String = "Hello,".Trim()
        Dim found As Boolean = DoesTokenEndWithASymbol(token)

        Assert.IsTrue(found, "The last character should be a symbol.")
    End Sub

    <TestMethod()> _
    Public Sub Chop_String()

        Dim truncated As String = Chop("Hello World")
        Assert.IsTrue(truncated = "Hello Worl", "The two values should be the same.")

    End Sub

    <TestMethod()> _
    Public Sub Process_Token_With_Symbol()
        Dim line As String = "No, Sam I am.  I do not like green eggs and ham."
        Dim tokens() As String = line.Split(" "c)
        Dim output As String = String.Empty

        If DoesTokenEndWithASymbol(tokens(0)) Then
            output = Chop(tokens(0))
        End If

        Assert.AreEqual("No", output, "The items should be the same.")

    End Sub

    <TestMethod()> _
    Public Sub Load_Words_Into_Dictionary()

        Dim line = "one two two three"
        LoadDictionary(line)

        Assert.AreEqual(3, dictionaryContainer.Count, "There should be three items in the dictionary.")
    End Sub

    <TestMethod()> _
    Public Sub Music_File_Has_Proper_Extension()
        Dim musicFile = "C:\Music\Amazon MP3\ABC\The Look Of Love - The Very Best Of ABC\01-15- Vanity Kills.mp3"
        Dim fi As FileInfo = New FileInfo(musicFile)
        Dim extension As String = fi.Extension
        Dim validExtensions = {".mp3"}

        Dim isValid = validExtensions.Any(Function(x) x = extension)
        Assert.IsTrue(isValid, "The music file has a valid extension")
    End Sub
#End Region

#Region "Working Stuff"
    <TestMethod()> _
    Public Sub Clean_All_Tokens_With_Punctuation()
        Dim tokens() As String = line.Split(New String() {" "c}, StringSplitOptions.RemoveEmptyEntries)
        Dim cleanTokens As List(Of String) = New List(Of String)()

        For Each item As String In tokens
            Dim base As String = String.Empty
            If DoesTokenEndWithASymbol(item) Then
                base = Chop(item)
            Else
                base = item
            End If
            cleanTokens.Add(base)
        Next

        Dim isPunctuationFree As Boolean = Not cleanTokens.Any(Function(e) DoesTokenEndWithASymbol(e))
    End Sub
    Private Function ValidateStartingLocation(path As String) As Boolean
        Return Directory.Exists(path)
    End Function

    Public Function ExtractFilenameExtensionFromString(filename As String) As String
        Dim extractedExtension As String = ""
        Dim dotIndex As Integer = 0
        If filename <> Nothing And filename.Contains(".") Then
            dotIndex = filename.LastIndexOf(".")
            extractedExtension = filename.Substring(dotIndex, filename.Length - dotIndex)
            If ValidateFileType(extractedExtension) Then
                Return extractedExtension
            End If
        End If
        Return Nothing
    End Function

    Public Function ValidateFileType(filetype As String) As Boolean
        Dim isValidExtension As Boolean = False
        Dim validExtensions As IEnumerable(Of String) = {".*", ".txt", ".html", ".xml"}

        isValidExtension = validExtensions.Any(Function(x) x = filetype)
        Return isValidExtension
    End Function

    Public Function ValidateFileExists(filename As String) As Boolean
        'Public Sub Validate_File_Exists(ByVal directory As String, ByVal filename As String)
        Dim isFileFound As Boolean = False
        Dim di As New IO.DirectoryInfo(_StartingLocation)
        Dim aryFi As IO.FileInfo() = di.GetFiles(filename)
        Dim fi As IO.FileInfo

        For Each fi In aryFi
            isFileFound = True
        Next

        Return isFileFound
    End Function

    Public Function ValidatePattern(pattern As String) As Boolean
        If pattern.Length > 0 Then
            Return True
        End If
        Return False
    End Function

    Private Function ValidateSingleFile(filepath As String) As Boolean
        'This function only works for a single file 
        'This function fails when a wildcard symbol is used to locate multiple files.
        Return File.Exists(filepath)
    End Function

    Private Function Chop(input As String) As String
        Dim temp As String = input.Trim()
        If temp.Length = 1 Then
            Return temp
        End If

        Return temp.Substring(0, temp.Length - 1)
    End Function

    Private Function DoesTokenEndWithASymbol(token As String) As Boolean
        Dim lastCharacter As String = token.Substring(token.Length - 1)
        Dim symbols() As String = {";", ",", "'", "."}
        Dim found As Boolean = False
        Dim index As Integer = 0

        While index < symbols.Length And Not found
            If symbols(index) = lastCharacter Then
                found = True
            End If
            index += 1
        End While

        Return found
    End Function
#End Region

    Private Sub LoadDictionary(line As String)
        Dim arrayOfStrings = line.Split(" "c)

        For Each word As String In arrayOfStrings
            If dictionaryContainer.ContainsKey(word) Then
                Dim wordCount = dictionaryContainer(word)
                dictionaryContainer(word) = wordCount + 1
            Else
                dictionaryContainer.Add(word, 1)
            End If
        Next
    End Sub

End Class
