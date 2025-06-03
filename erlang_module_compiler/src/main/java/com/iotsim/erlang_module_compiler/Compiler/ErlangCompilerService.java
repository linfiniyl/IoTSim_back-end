package com.iotsim.erlang_module_compiler.Compiler;

import com.iotsim.erlang_module_compiler.MessageDto;
import com.iotsim.erlang_module_compiler.MessageService;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Arrays;

import static com.iotsim.erlang_module_compiler.MessageService.*;

/**
        Сервис для компиляции Erlang файлов
 **/
@Slf4j
@Data
@RequiredArgsConstructor
@Component
public class ErlangCompilerService {

    @Value("${directory}")
    private String directory;

    /**
            * Записывает тело сообщения в файл
     * @param message Сообщение
     * @return {@code true} - если успешная запись, иначе возвращает {@code false}
     */
    private boolean writeToFile(MessageDto message) {
        try (FileOutputStream writer = new FileOutputStream(directory + message.getFileName())) {
            byte[] body = message.getMessage().getBody();
            writer.write(body);
        } catch (IOException e) {
            log.error("Error with writing in file {}", Arrays.toString(e.getStackTrace()));
            return false;
        }
        return true;
    }

    /**
     * Компилирует Erlang файл. Код извлекается из полученного сообщения и записывается в файл (.erl)
     * @param message Сообщение
     * @return Возращает статус операции.
     * {@link MessageService#COMPILATION_FAILED} - ошибка компиляции
     * {@link MessageService#COMPILATION_SUCCESS} - успешная компиляция
     * {@link MessageService#WRITING_FILE_ERROR} - ошибка записи в файл (.erl)
     */
    public String compileErlangFile(MessageDto message) {
        if (writeToFile(message)) {
            try {
                Process process = Runtime
                        .getRuntime()
                        .exec(String.format("erl -compile %s", message.getFileName()));
                try(var errors = process.errorReader()){
                    if (errors.ready()){
                        throw new IOException(Arrays.toString(errors.lines().toArray()));
                    }
                } catch (Exception processException){
                    throw new IOException(processException);
                }
            } catch (IOException e) {
                log.error("Error with compilation erlang file {}", Arrays.toString(e.getStackTrace()));
                return COMPILATION_FAILED;
            }
            return COMPILATION_SUCCESS;
        } else {
            return WRITING_FILE_ERROR;
        }
    }
}