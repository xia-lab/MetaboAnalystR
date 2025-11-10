package pro.metaboanalyst.workflows;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.type.TypeFactory;
import java.io.File;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import pro.metaboanalyst.lts.FunctionInfo;
import pro.metaboanalyst.utils.DataUtils;

public class FunctionInvoker {

    // Reusable ObjectMapper
    private static final ObjectMapper MAPPER = new ObjectMapper();

    // Registry for collection element types by property name
    // Add more entries here if you have other collection-typed properties.
    private static final Map<String, Class<?>> COLLECTION_ELEMENT_TYPES = new HashMap<>();

    static {
        COLLECTION_ELEMENT_TYPES.put("workflowOptions", pro.metaboanalyst.workflows.WorkflowParameters.class);
    }

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
        try {
            JsonNode rootNode = MAPPER.readTree(new File(filePath));

            Iterator<Map.Entry<String, JsonNode>> fields = rootNode.fields();
            while (fields.hasNext()) {
                Map.Entry<String, JsonNode> field = fields.next();
                String functionName = field.getValue().get("function").asText();
                JsonNode parameters = field.getValue().get("parameters");

                FunctionInfo functionInfo = new FunctionInfo(
                        functionName.split("\\.")[1],
                        functionName,
                        "Description" // TODO: plug in real description if needed
                );
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

            // Expect key format "beanName.propertyName"
            String[] parts = key.split("\\.");
            if (parts.length != 2) {
                System.out.println("Invalid key format: " + key);
                continue;
            }

            String propertyName = parts[1];
            String setterName = "set" + Character.toUpperCase(propertyName.charAt(0)) + propertyName.substring(1);

            Method setter = findSetterByName(clazz, setterName);

            if (setter == null) {
                System.out.println("Setter not found: " + clazz.getSimpleName() + "." + setterName);
                continue;
            }

            Class<?> paramType = setter.getParameterTypes()[0];
            try {
                Object convertedValue = convertValue(value, paramType, propertyName);
                setter.invoke(bean, convertedValue);
                /*
                System.out.println(clazz.getSimpleName() + "." + setterName
                        + " (param=" + paramType.getSimpleName() + ") invoked.");
*/
            } catch (Exception ex) {
                System.out.println("Failed to invoke setter " + clazz.getSimpleName() + "." + setterName
                        + " with value type " + (value == null ? "null" : value.getClass().getName())
                        + " -> " + ex.getMessage());
                throw ex;
            }
        }
    }

    private static Method findSetterByName(Class<?> clazz, String setterName) {
        for (Method m : clazz.getMethods()) {
            if (m.getName().equals(setterName) && m.getParameterCount() == 1) {
                return m;
            }
        }
        return null;
    }

    @SuppressWarnings("unchecked")
    private static Object convertValue(Object value, Class<?> targetType, String propertyName) {
        if (value == null) {
            return null;
        }
        if (targetType.isAssignableFrom(value.getClass())) {
            return value;
        }

        // Handle Set/List with known element type...
        if (Set.class.isAssignableFrom(targetType) || List.class.isAssignableFrom(targetType)) {
            Class<?> elemType = COLLECTION_ELEMENT_TYPES.get(propertyName);
            TypeFactory tf = MAPPER.getTypeFactory();

            if (elemType != null) {
                if (Set.class.isAssignableFrom(targetType)) {
                    return MAPPER.convertValue(value, tf.constructCollectionType(LinkedHashSet.class, elemType));
                } else {
                    return MAPPER.convertValue(value, tf.constructCollectionType(ArrayList.class, elemType));
                }
            }
            // Fallback best-effort conversion without element typing
            if (Set.class.isAssignableFrom(targetType)) {
                List<Object> list = MAPPER.convertValue(value, new TypeReference<List<Object>>() {
                });
                return new LinkedHashSet<>(list);
            } else {
                return MAPPER.convertValue(value, new TypeReference<List<Object>>() {
                });
            }
        }

        // --- FIXED: robust conversion to String[] ---
        if (targetType == String[].class) {
            // JSON array → List -> String[]
            if (value instanceof List<?>) {
                List<?> list = (List<?>) value;
                return list.stream().map(String::valueOf).toArray(String[]::new);
            }
            // Already some array type -> copy element-wise into String[]
            if (value.getClass().isArray()) {
                int len = java.lang.reflect.Array.getLength(value);
                String[] arr = new String[len];
                for (int i = 0; i < len; i++) {
                    Object el = java.lang.reflect.Array.get(value, i);
                    arr[i] = String.valueOf(el);
                }
                return arr;
            }
            // Scalar string: support either "a,b,c" or "[a, b, c]" (strip brackets and quotes)
            String s = value.toString().trim();
            if (s.startsWith("[") && s.endsWith("]")) {
                s = s.substring(1, s.length() - 1);
            }
            if (s.isEmpty()) {
                return new String[0];
            }
            return Arrays.stream(s.split("\\s*,\\s*"))
                    .map(FunctionInvoker::stripQuotes)
                    .toArray(String[]::new);
        }

        // Primitive/wrapper conversions via String
        String s = value.toString();
        if (targetType == int.class || targetType == Integer.class) {
            return Integer.valueOf(s);
        }
        if (targetType == double.class || targetType == Double.class) {
            return Double.valueOf(s);
        }
        if (targetType == boolean.class || targetType == Boolean.class) {
            return Boolean.valueOf(s);
        }
        if (targetType == long.class || targetType == Long.class) {
            return Long.valueOf(s);
        }
        if (targetType == float.class || targetType == Float.class) {
            return Float.valueOf(s);
        }
        if (targetType == short.class || targetType == Short.class) {
            return Short.valueOf(s);
        }
        if (targetType == byte.class || targetType == Byte.class) {
            return Byte.valueOf(s);
        }
        if (targetType == char.class || targetType == Character.class) {
            return s.isEmpty() ? null : s.charAt(0);
        }

        // For POJOs/Maps/JsonNode: let Jackson bind
        if (value instanceof Map || value instanceof List || value instanceof JsonNode) {
            return MAPPER.convertValue(value, targetType);
        }

        // Last resort: generic conversion
        try {
            return MAPPER.convertValue(value, targetType);
        } catch (IllegalArgumentException ignored) {
            return value;
        }
    }

// Helper to unquote tokens like "'mum'" or "\"mum\""
    private static String stripQuotes(String x) {
        if (x == null) {
            return null;
        }
        String t = x.trim();
        if ((t.startsWith("\"") && t.endsWith("\"")) || (t.startsWith("'") && t.endsWith("'"))) {
            return t.substring(1, t.length() - 1);
        }
        return t;
    }

    public static void invokeFunction(FunctionInfo functionInfo) throws Exception {
        // e.g., "myBean.doWork"
        String functionName = functionInfo.getFunction();
        String[] parts = functionName.split("\\.", 2);
        if (parts.length != 2) {
            throw new IllegalArgumentException("Invalid function format: " + functionName + " (expected beanName.methodName)");
        }

        String beanName = parts[0];
        String methodName = parts[1];

        // 1) Get the CDI/JSF bean (uses your new DataUtils.findBean)
        Object bean = DataUtils.findBean(beanName);
        if (bean == null) {
            throw new IllegalStateException("Bean not found: " + beanName);
        }

        // 2) Set parameters via setters on that bean
        Map<String, Object> parameters = functionInfo.getParameters();
        setParameters(bean, parameters);

        // 3) Invoke the no-arg method
        Method method = bean.getClass().getMethod(methodName);
        method.invoke(bean);
    }

    public static void callSetters(FunctionInfo functionInfo) throws Exception {
        String functionName = functionInfo.getFunction();
        Map<String, Object> parameters = functionInfo.getParameters();

        String beanName = functionName.split("\\.")[0];
        Object bean = DataUtils.findBean(beanName);

        setParameters(bean, parameters);
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

            Object paramValue = convertJsonNode(value);
            functionInfo.addParameter(key, paramValue);
        }
    }

    private static Object convertJsonNode(JsonNode node) {
        if (node == null || node.isNull()) {
            return null;
        }
        if (node.isTextual()) {
            return node.asText();
        }
        if (node.isInt()) {
            return node.asInt();
        }
        if (node.isBoolean()) {
            return node.asBoolean();
        }
        if (node.isDouble() || node.isFloat() || node.isBigDecimal()) {
            return node.asDouble();
        }
        if (node.isLong() || node.isBigInteger()) {
            return node.asLong();
        }

        if (node.isArray()) {
            // Return a List of Objects; convertValue() will bind to typed Set/List later
            return MAPPER.convertValue(node, new TypeReference<List<Object>>() {
            });
        }
        if (node.isObject()) {
            // Return a Map for POJO binding later
            return MAPPER.convertValue(node, new TypeReference<Map<String, Object>>() {
            });
        }
        // Fallback
        return MAPPER.convertValue(node, Object.class);
    }

    public static Map<String, FunctionInfo> loadFunctionInfosFromFile(String filePath) {
        try {
            Map<String, FunctionInfo> functionInfos = MAPPER.readValue(
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
        try {
            MAPPER.enable(SerializationFeature.INDENT_OUTPUT);
            MAPPER.writeValue(new File(filePath), functionInfos);
            System.out.println("FunctionInfos saved successfully to " + filePath);
        } catch (Exception e) {
            e.printStackTrace();
            System.err.println("Failed to save FunctionInfos to file.");
        }
    }
}
