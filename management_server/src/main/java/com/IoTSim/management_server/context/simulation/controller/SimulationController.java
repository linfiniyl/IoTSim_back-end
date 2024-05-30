package com.IoTSim.management_server.context.simulation.controller;

import com.IoTSim.management_server.context.device.model.DevicesAmount;
import com.IoTSim.management_server.context.simulation.model.Simulation;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.NoSuchElementException;
import java.util.Optional;

@RestController
public class SimulationController {
    @GetMapping("/{userId}/simulations")
    public List<Simulation> allSimulations(@PathVariable Long userId){
        return simulationService.findAllByUserId(userId);
    }
    @PutMapping("/{userId}/simulations/{id}")
    public ResponseEntity<?> updateSimulation(@RequestBody Simulation simulation, @PathVariable Long id, @PathVariable Long userId){
        Simulation simulationUpdate = simulationService.update(simulation, userId, id);
        if (simulationUpdate != null) {
            return new ResponseEntity<>(HttpStatus.OK);
        } else {
            return new ResponseEntity<>(HttpStatus.NOT_FOUND);
        }
    }
    @GetMapping("/{userId}/simulations/{id}")
    public ResponseEntity<Optional<Simulation>> getSimulation(@PathVariable Long id, @PathVariable Long userId){
        try{
            Optional<Simulation> simulation = simulationService.findById(userId, id);
            return new ResponseEntity<>(simulation, HttpStatus.OK);
        } catch (NoSuchElementException e){
            return new ResponseEntity<>(HttpStatus.NOT_FOUND);
        }
    }
    @DeleteMapping("/{userId}/simulation/{id}")
    public void deleteSimulation(@PathVariable Long id, @PathVariable Long userId){
        simulationService.deleteById(id, userId);
    }
    @PostMapping("/{userId}/simulations")
    public void addSimulation(@RequestBody Simulation simulation,  @PathVariable Long userId){
        simulation.setUserId(userId);// Костыль на случай если id пользователя не зарегает
        simulationService.createSimulation(simulation);
    }

    @GetMapping("/{userId}/simulations/{id}/entities")
    public List<DevicesAmount> allEntitiesAmount(@PathVariable Long userId, @PathVariable Long id){
        return entitiesAmountService.findAllById(userId, id);
    }
    @GetMapping("/{userId}/simulations/{id}/entities/{entityId}")
    public Optional<DevicesAmount> allEntitiesAmount(@PathVariable Long userId, @PathVariable Long id, @PathVariable Long entityId){
        return entitiesAmountService.findById(userId, id, entityId);
    }

    @DeleteMapping("/{userId}/simulations/{id}/entities/{entityId}")
    public void deleteEntityAmount(@PathVariable Long userId, @PathVariable Long id, @PathVariable Long entityId){
        entitiesAmountService.deleteById(userId, id, entityId);
    }
    @PostMapping("/{userId}/simulations/{id}/entities/{entityId}")
    public void addEntityAmount(@RequestBody DevicesAmount devicesAmount, @PathVariable Long userId, @PathVariable Long id, @PathVariable Long entityId){
        devicesAmount.setDeviceId(userId);
        devicesAmount.setDeviceId(id);
        devicesAmount.setDeviceId(entityId);
        entitiesAmountService.createEntitiesAmount(devicesAmount);
    }
    @PutMapping("/{userId}/simulations/{id}/entities/{entityId}")
    public ResponseEntity<?> updateEntitiesAmount(@RequestBody DevicesAmount devicesAmount, @PathVariable Long id, @PathVariable Long userId, @PathVariable Long entityId){
        DevicesAmount devicesAmountUpdate = entitiesAmountService.update(devicesAmount, userId, id, entityId);
        if (devicesAmountUpdate != null) {
            return new ResponseEntity<>(HttpStatus.OK);
        } else {
            return new ResponseEntity<>(HttpStatus.NOT_FOUND);
        }
    }
}
