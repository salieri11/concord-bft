package Servlets;

import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Component;

/**
 * This is a temporary utility class which allows us to get beans from
 * ApplicationContext in those classes which are not managed by Spring.
 * Once our servlets are managed by spring we can simply Autowire beans inside
 * them and this class can be removed.
 */
@Component
public class BeanUtil implements ApplicationContextAware {
    
    private static ApplicationContext applicationContext;
    
    @Override
    public void setApplicationContext(ApplicationContext appContext) throws BeansException {
        BeanUtil.applicationContext = appContext;
    }
    
    public static <T> T getBean(Class<T> beanClass) {
        return applicationContext.getBean(beanClass);
    }
}
