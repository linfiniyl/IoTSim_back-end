package executions_controller.com.code_generation_module.rabbitmq;

import org.springframework.amqp.core.*;
import org.springframework.amqp.rabbit.connection.CachingConnectionFactory;
import org.springframework.amqp.rabbit.connection.ConnectionFactory;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;


@Configuration
public class RabbitMQConfig {

    @Value("${spring.rabbitmq.host}")
    private String host;
    @Value("${spring.rabbitmq.port}")
    private Integer port;
    @Value("${spring.rabbitmq.username}")
    private String name;
    @Value("${spring.rabbitmq.password}")
    private String password;
    @Value("${spring.rabbitmq.queue.management_server}")
    private String queueNameMS;
    @Value("${spring.rabbitmq.queue.compilation_module}")
    private String queueNameCM;
    @Value("${spring.rabbitmq.queue.execution_nodes}")
    private String queueNameEN;
    @Value("${spring.rabbitmq.exchange}")
    private String exchangeName;
    @Value("${spring.rabbitmq.routing_key.execution_controller}")
    private String msRoutingKey;
    @Value("${spring.rabbitmq.routing_key.compilation_module}")
    private String cmRoutingKey;
    @Value("${spring.rabbitmq.routing_key.execution_nodes}")
    private String enRoutingKey;


    @Bean
    public ConnectionFactory connectionFactory(){
        CachingConnectionFactory cachingConnectionFactory = new CachingConnectionFactory();
        cachingConnectionFactory.setHost(host);
        cachingConnectionFactory.setPort(port);
        cachingConnectionFactory.setUsername(name);
        cachingConnectionFactory.setPassword(password);
        return cachingConnectionFactory;
    }

    @Bean
    public Queue getQueueMS(){
        return new Queue(queueNameMS, true);
    }

    @Bean
    public Queue getQueueCM(){
        return new Queue(queueNameCM, true);
    }
    @Bean
    public Queue getQueueEN(){
        return new Queue(queueNameEN, true);
    }

    @Bean
    public DirectExchange getExchange(){
        return new DirectExchange(exchangeName);
    }

    @Bean
    public Binding getBindingMS(DirectExchange exchange){
        return BindingBuilder.bind(getQueueMS()).to(exchange).with(msRoutingKey);
    }
    @Bean
    public Binding getBindingCM(DirectExchange exchange){
        return BindingBuilder.bind(getQueueCM()).to(exchange).with(cmRoutingKey);
    }
    @Bean
    public Binding getBindingEN(DirectExchange exchange){
        return BindingBuilder.bind(getQueueEN()).to(exchange).with(enRoutingKey);
    }

    @Bean
    public RabbitTemplate getTemplate(ConnectionFactory connectionFactory){
        RabbitTemplate rabbitTemplate = new RabbitTemplate(connectionFactory);
        rabbitTemplate.setExchange(exchangeName);
        return rabbitTemplate;
    }
}
