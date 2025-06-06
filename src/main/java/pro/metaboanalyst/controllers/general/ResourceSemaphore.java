/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.controllers.general;

import java.util.concurrent.Semaphore;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Named;

/**
 * Add throttling for CPU/RAM intensive tasks using semaphore Current setting:
 * 10 concurrent jobs. Reset every 10 min
 *
 * @author jeffxia
 */
@ApplicationScoped
@Named("semaphore")
public class ResourceSemaphore {

    // related  to the number of cores - 2 (GCCE n2-highmem-8)
    private static final int MAX_CONCURRENT_USES = 6;
    private static final int MAX_CONCURRENT_UPLOADS = 5;
    private static final int FIVE_MINUTES = 5 * 60 * 1000;
    private static final int TWE_MINUTES = 20 * 60 * 1000;
    private long startTimestamp, uploadStartTimestamp;
    private Semaphore semaphore, uploadSemaphore;

    public Semaphore getSemaphore() {

        if (semaphore == null) {
            semaphore = new Semaphore(MAX_CONCURRENT_USES, true);
            startTimestamp = System.currentTimeMillis();
        } else if (Math.abs(System.currentTimeMillis() - startTimestamp) > FIVE_MINUTES) {
            //reset after a time lapse to remove zombie processes (ie. cleaned by the system) 
            semaphore.drainPermits();
            semaphore.release(MAX_CONCURRENT_USES);
            startTimestamp = System.currentTimeMillis();
        }
        return semaphore;
    }

    public Semaphore getUploadSemaphore() {

        if (uploadSemaphore == null) {
            uploadSemaphore = new Semaphore(MAX_CONCURRENT_UPLOADS, true);
            uploadStartTimestamp = System.currentTimeMillis();
        } else if (Math.abs(System.currentTimeMillis() - uploadStartTimestamp) > TWE_MINUTES) {
            //reset after a time lapse to remove zombie processes (ie. cleaned by the system) 
            uploadSemaphore.drainPermits();
            uploadSemaphore.release(MAX_CONCURRENT_UPLOADS);
            uploadStartTimestamp = System.currentTimeMillis();
        }
        return uploadSemaphore;
    }

}
