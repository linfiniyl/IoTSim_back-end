package com.IoTSim.management_server.context.util;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.core.support.AbstractRepositoryMetadata;
import org.springframework.stereotype.Repository;

import java.util.Map;
import java.util.stream.Collectors;

public class RepositoryUtils {
    private static final Map<Class<?>,JpaRepository<?,?>> repositoryBeansMap = initializeRepositoryBeansMap();

    private static Map<Class<?>,JpaRepository<?,?>> initializeRepositoryBeansMap(){
        return ContextUtils.context.getBeansWithAnnotation(Repository.class)
                .values()
                .stream()
                .filter(obj -> obj instanceof JpaRepository<?,?>)
                .map(obj -> (JpaRepository<?,?>) obj)
                .collect(Collectors.toMap(
                        repo -> getModelRepositoryClass(repo.getClass()),
                        repo -> repo
                ));
    }

    public static <T, ID> T findObjectById(Class<T> objectType, ID id) {
        return RepositoryUtils.getRepositoryBean(objectType)
                .findById(id)
                .orElseThrow(ContextUtils.throwEntityNotFoundException(objectType));
    }

    private static Class<?> getModelRepositoryClass(Class<?> repositoryClass) {
        if (!repositoryClass.equals(JpaRepository.class)) {
            throw new ClassCastException();
        }
        return AbstractRepositoryMetadata.getMetadata(repositoryClass).getDomainType();
    }

    private static Class<?> getIdRepositoryClass(Class<?> repositoryClass) {
        if (!repositoryClass.equals(JpaRepository.class)) {
            throw new ClassCastException();
        }
        return AbstractRepositoryMetadata.getMetadata(repositoryClass).getIdType();
    }

    public static <T, ID> JpaRepository<T, ID> getRepositoryBean(Class<T> clazz) {
        JpaRepository<?, ?> repository = repositoryBeansMap.get(clazz);
        if (repository != null)
            return (JpaRepository<T, ID>) repository;
        else
            throw new RuntimeException();
    }
}
