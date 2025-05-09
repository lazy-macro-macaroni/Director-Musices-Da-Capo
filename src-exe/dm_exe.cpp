// dm_exe.cpp : This file contains the 'main' function. Program execution begins and ends there.
//
#include <Windows.h>
#include <iostream>
#include <filesystem>
#include <iterator>
#include <fstream>

namespace fs = std::filesystem;

// From https://stackoverflow.com/a/17387176
static std::string GetLastErrorAsString() {
    DWORD errorMessageID = GetLastError();

    //Get the error message ID, if any.
    if (errorMessageID == 0) {
        return std::string(); //No error message has been recorded
    }

    LPSTR messageBuffer = nullptr;

    //Ask Win32 to give us the string version of that message ID.
    //The parameters we pass in, tell Win32 to create the buffer that holds the message for us (because we don't yet know how long the message string will be).
    size_t size = FormatMessageA(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
        NULL, errorMessageID, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPSTR)&messageBuffer, 0, NULL);

    //Copy the error message into a std::string.
    std::string message(messageBuffer, size);

    //Free the Win32's string's buffer.
    LocalFree(messageBuffer);

    return message;
}

int main() {
    fs::path javaPath = fs::current_path();
    javaPath /= "jre";
    javaPath /= "bin";
    javaPath /= "java.exe";
    std::cout << "Java path is " << javaPath << '\n';

    fs::path cmdPath = fs::current_path();
    cmdPath /= "resources";
    cmdPath /= "java-command.txt";
    std::cout << "Java command path is " << cmdPath << '\n';

    std::ifstream inputFile(cmdPath);
    //auto ifs = std::ifstream(cmdPath.data());
    std::wstring cmd(std::istreambuf_iterator<char>{inputFile}, {});

    std::wcout << "Java command is " << cmd << '\n';

    STARTUPINFO si;
    PROCESS_INFORMATION pi;

    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    ZeroMemory(&pi, sizeof(pi));

    std::wstring command = L"";
    command += javaPath.wstring();
    command += L" ";
    command += cmd;

    wchar_t* commandWchar = new wchar_t[command.size() + 1];
    std::copy(command.begin(), command.end(), commandWchar);
    commandWchar[command.size()] = L'\0';

    // Start the child process. 
    if (!CreateProcess(NULL,   // No module name (use command line)
        commandWchar,        // Command line
        NULL,           // Process handle not inheritable
        NULL,           // Thread handle not inheritable
        FALSE,          // Set handle inheritance to FALSE
        0,              // No creation flags
        NULL,           // Use parent's environment block
        NULL,           // Use parent's starting directory 
        &si,            // Pointer to STARTUPINFO structure
        &pi)           // Pointer to PROCESS_INFORMATION structure
        )
    {
        std::string message = GetLastErrorAsString();
        std::cout << "CreateProcess failed: " << message << '\n';
        system("pause");
        return 1;
    }

    // Wait until child process exits.
    WaitForSingleObject(pi.hProcess, INFINITE);

    DWORD exitCode;
    BOOL exitCodeResult = GetExitCodeProcess(pi.hProcess, &exitCode);

    if (exitCodeResult == 0) {
        std::string message = GetLastErrorAsString();
        std::cout << "Could not get exit code: " << message << '\n';
        system("pause");
        return 1;
    }

    // Close process and thread handles. 
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);

    if (exitCode != 0) {
        std::wcout << "Java exited abnormally with code: " << exitCode << '\n';
        system("pause");
        return 1;
    }

    return 0;
}
