/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.contracts;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.file.DirectoryIteratorException;
import java.nio.file.DirectoryStream;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * A compiler class which allows compiling solidity contract source codes.
 */
public class Compiler {

    private static final Logger logger = LogManager.getLogger(Compiler.class);

    /**
     * Creates a temporary directory and creates a solidity contract source code file inside that directory. The output
     * files generated by the compiler will also be present in same temporary directory
     *
     * <p>@param contents solidity source code
     * @return The path of the created source file.
     */
    private static Path createSourceFiles(String contents) throws IOException {
        // Create a random directory
        // Need execute permissions to `cd` into that directory
        String permissions = "rwxrwxrwx";
        String directoryPrefix = "helen-solc";
        String sourceFileName = "source.sol";
        FileAttribute<Set<PosixFilePermission>> attr =
                PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString(permissions));
        Path workDir = Files.createTempDirectory(directoryPrefix, attr);
        // Note: Ideally we should name this file from the name of the
        // contract, i.e if contract is `ContractVersion Manager {..` then we
        // should name file as `Manager.sol` however that will require
        // parsing of the `contents` string. Instead we create a source file
        // with generic name and let the compiler compile this file.
        // compiler will actually parse the source code and generate output
        // files with proper names (Manager.bin, Manager_meta.json etc)
        // We can then use these file names to deduce the name of contract
        Path sourceFile = workDir.resolve(sourceFileName);
        Files.createFile(sourceFile, attr);
        try (BufferedWriter writer = Files.newBufferedWriter(sourceFile, Charset.defaultCharset())) {
            writer.write(contents);
        }
        return sourceFile;
    }

    /**
     * Recursively deletes the given directory tree rooted at `root`.
     *
     * @param root The Path of root directory which should be deleted.
     */
    private static void deleteDirectoryTree(Path root) {
        if (root == null || !Files.exists(root)) {
            return;
        }

        try {
            Files.walkFileTree(root, new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
                    if (exc == null) {
                        Files.delete(dir);
                    } else {
                        logger.warn("Exception while deleting: " + dir, exc);
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                    Files.delete(file);
                    return FileVisitResult.CONTINUE;
                }
            });
        } catch (Exception e) {
            logger.warn("Exception while deleting directory tree starting at: " + root, e);
        }
    }

    /**
     * Checks if the given Path represents a solidity compiler generated bytecode file. Currently the extension of
     * solidity compiler generated bytecode file is `.bin`.
     *
     * @param file Path of the bytecode file
     * @return returns true if file is a bytecode file, false otherwise
     */
    private static boolean isBytecodeFile(Path file) {
        return file.toString().endsWith(".bin");
    }

    /**
     * Checks if the given Path represents a solidity compiler generated metadata file. Currently solidity compiler
     * appends `_meta` to the name of the contract and then generates a metadata file with name as
     * `[contract_name]_meta.json`. Here we simply check if given path ends in `_meta.json`.
     *
     * @param file The Path of the metadata file
     * @return Returns true if file is a metadata file, false otherwise
     */
    private static boolean isMetadataFile(Path file) {
        return file.toString().endsWith("_meta.json");
    }

    /**
     * Extracts the name of the contract from given Path of the bytecode file ( [ContractName].bin) or from the given
     * Path of the metadata ([ContractName]_meta.json) file.
     *
     * @param file Path of the bytecode or metadata file
     * @return Name of the contract
     */
    private static String extractContractName(Path file) throws IllegalArgumentException {
        // If its a bytecode file then its name will be of the form
        // <ContractName>.bin. If its a metadata file then its name will be of
        // the form <ContractName>_meta.json
        String fileName = file.getFileName().toString();
        if (isBytecodeFile(file)) {
            return fileName.substring(0, fileName.lastIndexOf("."));
        } else if (isMetadataFile(file)) {
            return fileName.substring(0, fileName.lastIndexOf("_meta.json"));
        } else {
            throw new IllegalArgumentException("Only bytecode or metadata file " + "Path are supported");
        }
    }

    /**
     * Reads all contents of a file into a string. Note: The contents are first read into a byte array and then
     * converted into a string by using systems default charset.
     *
     * @param file Path of the file
     * @return Returns a string containing contents of the file
     */
    private static String readFileContents(Path file) throws IOException {
        byte[] bytes = Files.readAllBytes(file);
        return new String(bytes);
    }

    /**
     * Reads all the byteCode files in given directory and returns a Map with key as the name of the contract and value
     * as the contents of its bytecode (one map entry per bytecode file).
     *
     * @param dir The directory from which bytecode files should be read
     * @return The map of key-value pairs of contract name and its bytecode
     */
    private static Map<String, String> getCompiledByteCodeFrom(Path dir)
            throws IOException, DirectoryIteratorException {
        Map<String, String> byteCodeMap = new HashMap<>();
        try (DirectoryStream<Path> stream = Files.newDirectoryStream(dir)) {
            for (Path file : stream) {
                if (isBytecodeFile(file)) {
                    byteCodeMap.put(extractContractName(file), readFileContents(file));
                }
            }
        }
        return byteCodeMap;
    }

    /**
     * Reads all the metadata files in given directory and returns a Map with key as the name of the contract and value
     * as the contents of its metadata. There is one map entry per metadata file.
     *
     * @param dir The directory from which metadata files should be read
     * @return The map of key-value pairs of contract name and its metadata
     */
    private static Map<String, String> getMetadataFrom(Path dir) throws IOException, DirectoryIteratorException {
        Map<String, String> metadataMap = new HashMap<>();
        try (DirectoryStream<Path> stream = Files.newDirectoryStream(dir)) {
            for (Path file : stream) {
                if (isMetadataFile(file)) {
                    metadataMap.put(extractContractName(file), readFileContents(file));
                }
            }
        }
        return metadataMap;
    }

    /**
     * Reads the entire data available in the given input stream.
     *
     * @param is The input stream from which data should be read
     * @return Returns a string containing all the data of given input stream
     */
    private static String readStream(InputStream is) throws IOException {
        String line;
        BufferedReader reader = new BufferedReader(new InputStreamReader(is));
        StringBuilder sb = new StringBuilder();
        while ((line = reader.readLine()) != null) {
            sb.append(line);
        }
        return sb.toString();
    }

    /**
     * Compiles the given solidity contract code and returns the result of compilation. This method invokes the
     * pre-installed `solc` command. Hence, we first have to write the give solidity contract code into a file which we
     * then pass to `solc` as a command line argument.
     *
     * @param solidityCode The string containing solidity source code
     * @return The result object containing compilation result.
     */
    public static Result compile(String solidityCode) {
        Result result = new Result();
        Path sourceFile = null;
        try {
            sourceFile = createSourceFiles(solidityCode);
            // We need a command of form
            // solc --bin --metadata -o outputDir inputFile
            String[] command =
                {"solc", "--bin", "--metadata", "-o", sourceFile.getParent().toString(), sourceFile.toString()};

            Process compileProcess = Runtime.getRuntime().exec(command);
            // Get process output - in case of Process the inputStream
            // represents output generated by the process, and outputStream
            // represents the input given to the process
            final String stdOut = readStream(compileProcess.getInputStream());
            // Get process input
            final String stdErr = readStream(compileProcess.getErrorStream());

            compileProcess.waitFor();
            // get process exit code
            int exitCode = compileProcess.exitValue();

            if (exitCode == 0) {
                result.setSuccess(true);
                // read bin and json files for bytecode and metadata
                result.setByteCodeMap(getCompiledByteCodeFrom(sourceFile.getParent()));
                result.setMetadataMap(getMetadataFrom(sourceFile.getParent()));
            }

            result.setStdout(stdOut);
            result.setStderr(stdErr);

        } catch (IOException | InterruptedException e) {
            logger.warn("Error in compilation:" + e);
        } catch (Exception e) {
            logger.warn("Other than IO error", e);
            throw e;
        } finally {
            // Delete the created source files
            deleteDirectoryTree(sourceFile.getParent());
        }
        return result;
    }

    /**
     * A simple POJO class representing the Result of solidity compilation.
     */
    public static class Result {
        // compilation success or failure
        boolean success;
        // standard output of compilation
        String stdout;
        // standard error of compilation
        String stderr;
        // Bytecode of the compiled contract, if successful
        Map<String, String> byteCodeMap;
        // Metadata of the compiled contract, if successful
        Map<String, String> metadataMap;

        public boolean isSuccess() {
            return success;
        }

        public void setSuccess(boolean success) {
            this.success = success;
        }

        public String getStdout() {
            return stdout;
        }

        public void setStdout(String stdout) {
            this.stdout = stdout;
        }

        public String getStderr() {
            return stderr;
        }

        public void setStderr(String stderr) {
            this.stderr = stderr;
        }

        public Map<String, String> getByteCodeMap() {
            return byteCodeMap;
        }

        public void setByteCodeMap(Map<String, String> byteCodeMap) {
            this.byteCodeMap = byteCodeMap;
        }

        public Map<String, String> getMetadataMap() {
            return metadataMap;
        }

        public void setMetadataMap(Map<String, String> metadataMap) {
            this.metadataMap = metadataMap;
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("Compilation Success: ").append(success).append("\n").append("stdout: ").append(stdout)
                    .append("\n").append("stderr: ").append(stderr).append("\n");
            if (byteCodeMap != null && metadataMap != null) {
                for (Map.Entry<String, String> e : byteCodeMap.entrySet()) {
                    sb.append(e.getKey() + "=> [" + e.getValue() + "]");
                }
                for (Map.Entry<String, String> e : metadataMap.entrySet()) {
                    sb.append(e.getKey() + "=> [" + e.getValue() + "]");
                }
            }
            return sb.toString();
        }
    }

}
