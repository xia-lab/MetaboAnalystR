package pro.metaboanalyst.workflows;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import java.io.File;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import jakarta.faces.context.FacesContext;
import pro.metaboanalyst.lts.FunctionInfo;

public class FunctionInvoker {

    public static void invokeFunctionsFromHashMap(HashMap<String, FunctionInfo> functionInfos) {
        try {
            for (Map.Entry<String, FunctionInfo> entry : functionInfos.entrySet()) {
                FunctionInfo functionInfo = entry.getValue();
                invokeFunction(functionInfo);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void invokeFunctionsFromFile(String filePath) {
        ObjectMapper mapper = new ObjectMapper();
        try {
            JsonNode rootNode = mapper.readTree(new File(filePath));

            Iterator<Map.Entry<String, JsonNode>> fields = rootNode.fields();
            while (fields.hasNext()) {
                Map.Entry<String, JsonNode> field = fields.next();
                String functionName = field.getValue().get("function").asText();
                JsonNode parameters = field.getValue().get("parameters");

                // Invoke the main function
                FunctionInfo functionInfo = new FunctionInfo(
                        functionName.split("\\.")[1],
                        functionName,
                        "Description"); // Add your description logic here
                addParametersToFunctionInfo(functionInfo, parameters);
                invokeFunction(functionInfo);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void setParameters(Object bean, Map<String, Object> parameters) throws Exception {
        if (bean == null || parameters == null) {
            return;
        }

        Class<?> clazz = bean.getClass();
        for (Map.Entry<String, Object> entry : parameters.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            // Assumes the key format is "beanName.propertyName"
            String[] parts = key.split("\\.");
            if (parts.length != 2) {
                System.out.println("Invalid key format: " + key);
                continue;
            }

            String propertyName = parts[1];  // "vcFcThresh"

            String setterName = "set" + Character.toUpperCase(propertyName.charAt(0)) + propertyName.substring(1);
            System.out.println(bean.getClass().getSimpleName() + "." + setterName + "======setterName");  // Corrected log for clarity

            if (value == null) {
                System.out.println("Value null: " + setterName);

            } else {
                Method setter = findMethod(clazz, setterName, value);

                if (setter != null) {
                    Object convertedValue = convertValue(value, setter.getParameterTypes()[0]);
                    setter.invoke(bean, convertedValue);
                } else {
                    System.out.println("Setter not found: " + setterName);
                }
            }
        }
    }

    private static Method findMethod(Class<?> clazz, String methodName, Object value) {
        Class<?> valueType = value.getClass();
        try {
            return clazz.getMethod(methodName, valueType);
        } catch (NoSuchMethodException e) {
            // Try to find a method with primitive type if valueType is a wrapper class
            if (valueType == Integer.class || valueType == String.class) {
                try {
                    return clazz.getMethod(methodName, int.class);
                } catch (NoSuchMethodException ignored) {
                }
            } else if (valueType == Double.class || valueType == String.class) {
                try {
                    return clazz.getMethod(methodName, double.class);
                } catch (NoSuchMethodException ignored) {
                }
            } else if (valueType == Boolean.class || valueType == String.class) {
                try {
                    return clazz.getMethod(methodName, boolean.class);
                } catch (NoSuchMethodException ignored) {
                }
            } else if (valueType == Long.class || valueType == String.class) {
                try {
                    return clazz.getMethod(methodName, long.class);
                } catch (NoSuchMethodException ignored) {
                }
            } else if (valueType == Float.class || valueType == String.class) {
                try {
                    return clazz.getMethod(methodName, float.class);
                } catch (NoSuchMethodException ignored) {
                }
            } else if (valueType == Short.class || valueType == String.class) {
                try {
                    return clazz.getMethod(methodName, short.class);
                } catch (NoSuchMethodException ignored) {
                }
            } else if (valueType == Byte.class || valueType == String.class) {
                try {
                    return clazz.getMethod(methodName, byte.class);
                } catch (NoSuchMethodException ignored) {
                }
            } else if (valueType == Character.class || valueType == String.class) {
                try {
                    return clazz.getMethod(methodName, char.class);
                } catch (NoSuchMethodException ignored) {
                }
            } else if (List.class.isAssignableFrom(valueType)) {
                try {
                    return clazz.getMethod(methodName, List.class);
                } catch (NoSuchMethodException ignored) {
                }
            } else if (valueType.isArray() && valueType.getComponentType() == String.class) {
                try {
                    return clazz.getMethod(methodName, String[].class);
                } catch (NoSuchMethodException ignored) {
                }
            }
        }
        return null;
    }

    private static Object convertValue(Object value, Class<?> targetType) {
        if (value == null) {
            return null;
        }
        if (targetType.isAssignableFrom(value.getClass())) {
            return value;
        }

        String stringValue = value.toString();

        if (targetType == int.class || targetType == Integer.class) {
            return Integer.valueOf(stringValue);
        } else if (targetType == double.class || targetType == Double.class) {
            return Double.valueOf(stringValue);
        } else if (targetType == boolean.class || targetType == Boolean.class) {
            return Boolean.valueOf(stringValue);
        } else if (targetType == long.class || targetType == Long.class) {
            return Long.valueOf(stringValue);
        } else if (targetType == float.class || targetType == Float.class) {
            return Float.valueOf(stringValue);
        } else if (targetType == short.class || targetType == Short.class) {
            return Short.valueOf(stringValue);
        } else if (targetType == byte.class || targetType == Byte.class) {
            return Byte.valueOf(stringValue);
        } else if (targetType == char.class || targetType == Character.class) {
            return stringValue.charAt(0);
        } else if (targetType == String[].class) {
            return stringValue.split("\\s*,\\s*");
        } else if (List.class.isAssignableFrom(targetType) && targetType.isAssignableFrom(ArrayList.class)) {
            return new ArrayList<>(Arrays.asList(stringValue.split("\\s*,\\s*")));
        }
        return value;
    }

    private static boolean isCompatibleType(Object value, Class<?> paramType) {
        if (value == null) {
            return !paramType.isPrimitive();
        }
        // Basic compatibility check, expand as needed
        return paramType.isInstance(value)
                || (paramType.isPrimitive() && wrapPrimitive(paramType).isInstance(value));
    }

    private static Class<?> wrapPrimitive(Class<?> type) {
        if (type == int.class) {
            return Integer.class;
        }
        if (type == boolean.class) {
            return Boolean.class;
        }
        // Add other primitives as necessary
        return type;
    }

    public static void invokeFunction(FunctionInfo functionInfo) throws Exception {
        String functionName = functionInfo.getFunction();
        Map<String, Object> parameters = functionInfo.getParameters();

        // Get the bean instance to call methods on
        String beanName = functionName.split("\\.")[0];
        Object bean = getBeanInstance(beanName);

        // Set parameters using setters
        setParameters(bean, parameters);

        // Invoke the main function
        String methodName = functionName.split("\\.")[1];
        Class<?> clazz = bean.getClass();
        Method method = clazz.getMethod(methodName);
        method.invoke(bean);
    }

    public static void callSetters(FunctionInfo functionInfo) throws Exception {
        String functionName = functionInfo.getFunction();
        Map<String, Object> parameters = functionInfo.getParameters();

        // Get the bean instance to call methods on
        String beanName = functionName.split("\\.")[0];
        System.out.println(beanName + "===callSettersBeanName");
        Object bean = getBeanInstance(beanName);

        // Set parameters using setters
        setParameters(bean, parameters);

        // Invoke the main function
        //String methodName = functionName.split("\\.")[1];
        //Class<?> clazz = bean.getClass();
    }

    private static Object getBeanInstance(String beanName) {
        // This method should return an instance of the bean based on the className part of the function name.
        FacesContext context = FacesContext.getCurrentInstance();
        return context.getApplication().evaluateExpressionGet(context, "#{" + beanName + "}", Object.class);
    }

    public static void main(String[] args) {
        FunctionInvoker invoker = new FunctionInvoker();
        invoker.invokeFunctionsFromFile("path_to_your_json_file.json");
    }

    private static void addParametersToFunctionInfo(FunctionInfo functionInfo, JsonNode parameters) {
        if (parameters == null || !parameters.isObject()) {
            return;
        }

        Iterator<Map.Entry<String, JsonNode>> it = parameters.fields();
        while (it.hasNext()) {
            Map.Entry<String, JsonNode> entry = it.next();
            String key = entry.getKey();
            JsonNode value = entry.getValue();

            // Convert JsonNode to an appropriate Java object
            Object paramValue = convertJsonNode(value);
            functionInfo.addParameter(key, paramValue);
        }
    }

    private static Object convertJsonNode(JsonNode node) {
        // This method converts a JsonNode to a suitable Java object based on the content of the node.
        // Adjust the logic based on what types of parameters your application might expect.
        if (node.isTextual()) {
            return node.asText();
        } else if (node.isInt()) {
            return node.asInt();
        } else if (node.isBoolean()) {
            return node.asBoolean();
        } else if (node.isDouble()) {
            return node.asDouble();
        } else if (node.isArray()) {
            // Convert arrays as needed, possibly to List or similar
            return node.toString(); // Simplistic handling, adjust as necessary
        } else if (node.isObject()) {
            // Convert objects as needed, possibly to Map or similar
            return node.toString(); // Simplistic handling, adjust as necessary
        }
        return null; // Return null for nodes that cannot be directly converted
    }

    public static Map<String, FunctionInfo> loadFunctionInfosFromFile(String filePath) {
        ObjectMapper mapper = new ObjectMapper();
        try {
            // Read the JSON file into a Map<String, FunctionInfo>
            Map<String, FunctionInfo> functionInfos = mapper.readValue(
                    new File(filePath),
                    new TypeReference<Map<String, FunctionInfo>>() {
            }
            );
            System.out.println("FunctionInfos loaded successfully from " + filePath);
            return functionInfos;
        } catch (Exception e) {
            e.printStackTrace();
            System.err.println("Failed to load FunctionInfos from file.");
            return null;
        }
    }
    
    public static void saveFunctionInfosToFile(Map<String, FunctionInfo> functionInfos, String filePath) {
        ObjectMapper mapper = new ObjectMapper();
        // Enable pretty-printing (optional)
        mapper.enable(SerializationFeature.INDENT_OUTPUT);

        try {
            // Write the map to the file
            mapper.writeValue(new File(filePath), functionInfos);
            System.out.println("FunctionInfos saved successfully to " + filePath);
        } catch (Exception e) {
            e.printStackTrace();
            System.err.println("Failed to save FunctionInfos to file.");
        }
    }
}
