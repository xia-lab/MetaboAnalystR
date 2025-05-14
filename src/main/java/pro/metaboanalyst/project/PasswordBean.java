/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.project;

import java.math.BigInteger;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.spec.InvalidKeySpecException;
import java.util.Random;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;

/**
 *
 * @author qiang
 */
public class PasswordBean {

    private static final Random RANDOM = new SecureRandom();
    private static final String ALPHABET = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

    public static String getSalt(int length) {
        StringBuilder returnValue = new StringBuilder(length);
        for (int i = 0; i < length; i++) {
            returnValue.append(ALPHABET.charAt(RANDOM.nextInt(ALPHABET.length())));
        }
        return new String(returnValue);
    }

    public static boolean verifyUserPassword(String providedPassword,
            String securedPassword, String salt) {
        boolean returnValue = false;

        // Generate New secure password with the same salt
        String newSecurePassword = generateSecurePassword(providedPassword, salt);

        // Check if two passwords are equal
        returnValue = newSecurePassword.equalsIgnoreCase(securedPassword);

        return returnValue;
    }

    public static String generateSecurePassword(String password, String salt) {
//        System.out.println("generateSecurePassword - password --> " + password);
//        System.out.println("generateSecurePassword - salt     --> " + salt);

        char[] chars = password.toCharArray();
        byte[] saltBytes = salt.getBytes();

        PBEKeySpec spec = new PBEKeySpec(chars, saltBytes, 64000, 256);
        SecretKeyFactory skf = null;
        try {
            skf = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256");
        } catch (NoSuchAlgorithmException ex) {
            Logger.getLogger(PasswordBean.class.getName()).log(Level.SEVERE, null, ex);
        }
        byte[] hash = null;
        try {
            hash = skf.generateSecret(spec).getEncoded();
        } catch (InvalidKeySpecException ex) {
            Logger.getLogger(PasswordBean.class.getName()).log(Level.SEVERE, null, ex);
        }
        BigInteger bi = new BigInteger(1, hash);
        return bi.toString(16);
    }
}
