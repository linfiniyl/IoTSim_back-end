package com.iotsim.erlang_module_compiler.Compiler;

import org.springframework.amqp.core.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;

@Component
public class ErlangCompilerService {
    private String fileName;
    private Long entityId;
    private Long attributeId;
    private Message message;
    private Long userId;
    private Long simulationId;
    private Long entityNumber;
    private Boolean successWriting = false;
    private Boolean isWindows = System.getProperty("os.name").toLowerCase().startsWith("windows");


    public ErlangCompilerService(){
    }

    private boolean writeToFile(){
        try( FileOutputStream writer = new FileOutputStream(fileName)){
            byte[] body = message.getBody();
            writer.write(body);
        } catch(IOException e){
            e.printStackTrace();
            return false;
        }
        return true;
    }

    public int compileErlangFile(){
        successWriting = writeToFile();
        if (successWriting){
            Process process;
            if (isWindows){
                try {
                    process = Runtime.getRuntime().exec(String.format("erl.exe -compile %s", fileName));
                } catch (IOException e){
                    e.printStackTrace();
                    return -1;
                }
                return 0;
            } else {
                try {
                    process = Runtime.getRuntime().exec(String.format("erl.exe -compile %s", fileName));
                } catch (IOException e){
                    e.printStackTrace();
                    return -1;
                }
                return 0;
            }
        } else {
            return -1;
        }

    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public Long getEntityId() {
        return entityId;
    }

    public void setEntityId(Long entityId) {
        this.entityId = entityId;
    }

    public Long getAttribute() {
        return attributeId;
    }

    public void setAttribute(Long attributeId) {
        this.attributeId = attributeId;
    }

    public Message getMessage() {
        return message;
    }

    public void setMessage(Message message) {
        this.entityId = message.getMessageProperties().getHeader("entityName");
        this.attributeId = message.getMessageProperties().getHeader("attributeName");
        this.userId = message.getMessageProperties().getHeader("userId");
        this.simulationId = message.getMessageProperties().getHeader("simulationId");
        this.entityNumber = message.getMessageProperties().getHeader("entityNumber");
        fileName = "${Directory}" + "${file_name}" + "_" + userId + "_" + simulationId + "_" +
                entityId +"_" + entityNumber + "_" + attributeId;
        this.message = message;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public Long getSimulationId() {
        return simulationId;
    }

    public void setSimulationId(Long simulationId) {
        this.simulationId = simulationId;
    }

    public Long getEntityNumber() {
        return entityNumber;
    }

    public void setEntityNumber(Long entityNumber) {
        this.entityNumber = entityNumber;
    }

    public Boolean getSuccessWriting() {
        return successWriting;
    }

    public void setSuccessWriting(Boolean successWriting) {
        this.successWriting = successWriting;
    }

    public Boolean getWindows() {
        return isWindows;
    }

    public void setWindows(Boolean windows) {
        isWindows = windows;
    }
}
